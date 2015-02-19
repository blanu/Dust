package Dust

import (
	"net"

	"github.com/blanu/Dust/go/Dust/proc"
)

// ClientDialer indicates how to dial new connections for the client side of a Dust connection group.
type ClientDialer interface {
	Dial(addr net.Addr) (Socket, error)
}

// clientGroup is a group that can actively add new connections by dialing them.
type clientGroup struct {
	group

	cliLink proc.Link

	spub   *ServerPublic
	dialer ClientDialer

	ackQuiescent     chan struct{}
	sigNoConnections chan struct{}
}

func (cg *clientGroup) initClientGroup(spub *ServerPublic, dialer ClientDialer) {
	cg.initGroup()
	cg.cliLink.InitLink(cg.runDialing)
	cg.spub = spub
	cg.dialer = dialer
	cg.discipline = cg
	cg.ackQuiescent = make(chan struct{}, 1)
	cg.sigNoConnections = make(chan struct{}, 1)
}

func (cg *clientGroup) newConnection() *groupedConnection {
	gconn := &groupedConnection{}
	gconn.initGroupedConnection()

	err := gconn.initClient(cg.spub, gconn)
	if err != nil {
		return nil
	}

	go func() {
		var sock Socket
		started := false
		defer func() {
			if !started {
				gconn.Closed()
				if sock != nil {
					sock.Close()
				}
			}
		}()

		sock, err := cg.dialer.Dial(cg.spub.tcpAddr)
		if err != nil {
			return
		}

		select {
		case _ = <-cg.cliLink.Kill:
			return
		case _ = <-gconn.sigStart:
		}

		err = gconn.spawn(sock)
		started = err == nil
	}()

	return gconn
}

func (cg *clientGroup) runQuiescent() {
	// Consume the first datagram sent to "anywhere" and force it to start a new connection.  (Addressed
	// datagrams won't trigger this because they get sent straight to outDispatch.)
	select {
	case _ = <-cg.cliLink.Kill:
	case agram := <-cg.outAnyGrams:
		cg.outDispatch <- agram
	}

	cg.ackQuiescent <- struct{}{}
}

func (cg *clientGroup) noConnections() {
	select {
	default:
	case cg.sigNoConnections <- struct{}{}:
	}
}

func (cg *clientGroup) runDialing() error {
	go cg.runQuiescent()
	quiescentRunning := true

	for {
		select {
		case _ = <-cg.cliLink.Kill:
			return nil
		case _ = <-cg.ackQuiescent:
			quiescentRunning = false
		case _ = <-cg.sigNoConnections:
			if !quiescentRunning {
				go cg.runQuiescent()
				quiescentRunning = true
			}
		}
	}
}

func (cg *clientGroup) spawn() {
	cg.groupLink.Spawn()
	cg.cliLink.Spawn()
}
