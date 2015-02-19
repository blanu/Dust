package Dust

import (
	"io"
	"net"

	"github.com/blanu/Dust/go/Dust/buf"
	"github.com/blanu/Dust/go/Dust/proc"
)

type (
	addressedGram struct {
		addr LinkAddr
		data []byte
		ack  chan gramAcknowledgment
	}

	gramAcknowledgment struct {
		addr LinkAddr
		n    int
		err  error
	}
)

func (agram addressedGram) unroutable() {
	if agram.ack != nil {
		agram.ack <- gramAcknowledgment{agram.addr, 0, ErrUnreachable}
	}
}

func (agram addressedGram) acknowledgeWith(n int, err error) {
	if agram.ack != nil {
		agram.ack <- gramAcknowledgment{agram.addr, n, err}
	}
}

func (agram addressedGram) acknowledge() {
	agram.acknowledgeWith(len(agram.data), nil)
}

// groupedConnection is a connection that is part of a connection group.
type groupedConnection struct {
	connection

	// Incoming datagrams are delivered to the funnel inAnyGrams.  Outgoing datagrams come from any
	// combination of the shared channel outAnyGrams and the connection-specific funnel outExactGrams.
	// All of these are set by the connection group when adding the connection.
	inAnyGrams    chan addressedGram
	outAnyGrams   chan addressedGram
	outExactGrams chan addressedGram

	// sigStart is triggered after the groupedConnection is fully added to the group, all the channel
	// values above are prepared, and any connection-specific processes can begin using the
	// groupedConnection as a crypting.Front.
	sigStart chan struct{}

	// graveChan is where the groupedConnection sends itself when it's closed.  The group couples this to
	// the group's shared deadConns chan when adding the connection.
	graveChan chan *groupedConnection
}

func (gconn *groupedConnection) PullGram(write func(p []byte, mayRetain bool)) {
	var agram addressedGram
	select {
	case agram = <-gconn.outExactGrams:
	case agram = <-gconn.outAnyGrams:
	default:
		return
	}

	// TODO: do we want to propagate errors here?  Hmm.
	write(agram.data, false)
	agram.acknowledge()
}

func (gconn *groupedConnection) PushGram(p []byte, mayRetain bool) {
	if !mayRetain {
		p = buf.CopyNew(p)
	}

	agram := addressedGram{gconn.remote, p, nil}
	select {
	case gconn.inAnyGrams <- agram:
	default:
		// Drop it on the floor.
	}
}

func (gconn *groupedConnection) drainAll() {
	for agram := range gconn.outExactGrams {
		agram.unroutable()
	}
}

func (gconn *groupedConnection) Closed() {
	go gconn.drainAll()
	gconn.graveChan <- gconn
}

func (gconn *groupedConnection) DrainOutput() {
	for {
		select {
		case agram := <-gconn.outExactGrams:
			agram.unroutable()
		default:
			return
		}
	}
}

func (gconn *groupedConnection) initGroupedConnection() {
	gconn.sigStart = make(chan struct{}, 1)
	gconn.graveChan = make(chan *groupedConnection, 1)
}

// groupDiscipline determines how a group handles connections.  Each group has one of these.
type groupDiscipline interface {
	newConnection() *groupedConnection
	noConnections()
}

// zeroGroupDiscipline is the most trivial group discipline.  It is incapable of making new connections, and
// does nothing when any connection-related events occur.
type zeroGroupDiscipline struct{}

func (_ zeroGroupDiscipline) newConnection() *groupedConnection {
	// No way of making new connections.
	return nil
}

func (_ zeroGroupDiscipline) noConnections() {
	// No need to do anything when no connections are present.
}

// group holds zero or more underlying Dust connections that should be treated together.
type group struct {
	groupLink proc.Link

	hardClose bool

	// Maps from remote token to connection.  We only ever transact with library clients regarding remote
	// addresses, really; the local addresses are mostly dummy tokens to allow comparison.
	byRemote map[LinkAddr]*groupedConnection
	count    int

	// inAnyGrams is a funnel that receives datagrams from all connections at once.  outAnyGrams is a
	// shared channel from which any connection may pull datagrams.  outDispatch receives datagrams to be
	// addressed to specific connections; as a special case, dispatching to the zero address means a new
	// connection is intended.  outBroadcast receives datagrams to be sent to all active connections; the
	// group controller iterates over the connections transmitting it.
	inAnyGrams                             chan addressedGram
	outAnyGrams, outDispatch, outBroadcast chan addressedGram

	// inConns receives new connections to be added to the group.  deadConns receives connections that
	// have terminated and should be removed from the group.  discipline determines how various connection
	// states should be handled; it receives callbacks from the group controller.
	inConns    chan *groupedConnection
	deadConns  chan *groupedConnection
	discipline groupDiscipline
}

func (g *group) initGroup() {
	g.byRemote = make(map[LinkAddr]*groupedConnection)
	g.inAnyGrams = make(chan addressedGram, 4)
	g.outAnyGrams = make(chan addressedGram, 4)
	g.outDispatch = make(chan addressedGram, 4)
	g.outBroadcast = make(chan addressedGram, 4)
	g.inConns = make(chan *groupedConnection, 1)
	g.deadConns = make(chan *groupedConnection, 1)
	g.discipline = new(zeroGroupDiscipline)
	g.groupLink.InitLink(g.runGroup)
}

func (g *group) addConnection(gconn *groupedConnection) {
	if _, present := g.byRemote[gconn.remote]; present {
		panic("Dust: adding connection to group twice")
	}

	gconn.inAnyGrams = g.inAnyGrams
	gconn.outAnyGrams = g.outAnyGrams
	gconn.outExactGrams = make(chan addressedGram, 1)
	go func() { g.deadConns <- <-gconn.graveChan }()

	g.byRemote[gconn.remote] = gconn
	g.count++
	gconn.sigStart <- struct{}{}
}

func (g *group) addNewConnection() (gconn *groupedConnection) {
	gconn = g.discipline.newConnection()
	if gconn != nil {
		g.addConnection(gconn)
	}
	return
}

func (g *group) removeConnection(gconn *groupedConnection) {
	if found := g.byRemote[gconn.remote]; found != gconn {
		panic("Dust: removing weird connection")
	}

	delete(g.byRemote, gconn.remote)
	g.count--
	if g.count == 0 {
		g.discipline.noConnections()
	}

	close(gconn.outExactGrams)
}

func (g *group) findConnection(addr LinkAddr) *groupedConnection {
	return g.byRemote[addr]
}

func (g *group) dispatch(agram addressedGram) {
	var gconn *groupedConnection
	if agram.addr.IsZero() {
		// Special case: requesting a new connection.
		gconn = g.addNewConnection()
	} else {
		gconn = g.findConnection(agram.addr)
	}

	if gconn != nil {
		gconn.outExactGrams <- agram
	} else {
		agram.unroutable()
	}
}

func (g *group) broadcast(agram addressedGram) {
	copiedData := buf.CopyNew(agram.data)
	agram.acknowledge()
	for _, gconn := range g.byRemote {
		gconn.outExactGrams <- addressedGram{gconn.remote, copiedData, nil}
	}
}

func (g *group) runGroup() error {
	for {
		select {
		case _ = <-g.groupLink.Kill:
			var err error
			if g.hardClose {
				for _, gconn := range g.byRemote {
					gconn.HardClose()
				}
			}
			return err

		case agram := <-g.outDispatch:
			g.dispatch(agram)
		case agram := <-g.outBroadcast:
			g.broadcast(agram)
		case gconn := <-g.inConns:
			g.addConnection(gconn)
		case gconn := <-g.deadConns:
			g.removeConnection(gconn)
		}
	}
}

func (g *group) Close() error {
	return g.groupLink.CloseWait()
}

func (g *group) HardClose() error {
	g.hardClose = true
	return g.groupLink.CloseWait()
}

func (g *group) ReadFromDust(p []byte) (n int, addr LinkAddr, err error) {
	agram, ok := <-g.inAnyGrams
	if !ok {
		err = io.EOF
		return
	}

	addr = agram.addr
	n = copy(p, agram.data)
	if n < len(agram.data) {
		err = io.ErrShortBuffer
	}
	agram.acknowledgeWith(n, err)
	return
}

func (g *group) ReadFrom(p []byte) (n int, addr net.Addr, err error) {
	n, addrVal, err := g.ReadFromDust(p)
	addr = &addrVal
	return
}

func (g *group) Read(p []byte) (n int, err error) {
	n, _, err = g.ReadFromDust(p)
	return
}

func (g *group) WriteToDust(p []byte, addr LinkAddr) (n int, err error) {
	var outChan chan<- addressedGram
	if addr.IsZero() {
		outChan = g.outAnyGrams
	} else {
		outChan = g.outDispatch
	}

	// Since we're using acknowledgment channels, we should be able to do this without copying p, right?
	ack := make(chan gramAcknowledgment, 1)
	agram := addressedGram{addr, p, ack}
	outChan <- agram
	result := <-ack
	n, err = result.n, result.err
	return
}

func (g *group) WriteToAnyDust(p []byte) (n int, addr LinkAddr, err error) {
	ack := make(chan gramAcknowledgment, 1)
	agram := addressedGram{LinkAddr{}, p, ack}
	g.outAnyGrams <- agram
	result := <-ack
	return result.n, result.addr, result.err
}

func (g *group) WriteToAllDust(p []byte) (n int, err error) {
	ack := make(chan gramAcknowledgment, 1)
	agram := addressedGram{LinkAddr{}, p, ack}
	g.outBroadcast <- agram
	result := <-ack
	return result.n, result.err
}

func (g *group) WriteTo(p []byte, addr net.Addr) (n int, err error) {
	var linkAddr LinkAddr
	switch a := addr.(type) {
	case nil:
		// Zero address is zero.
	case *LinkAddr:
		linkAddr = *a
	default:
		err = ErrAfNoSupport
		return
	}

	return g.WriteToDust(p, linkAddr)
}

func (g *group) Write(p []byte) (n int, err error) {
	err = ErrUnreachable
	return
}
