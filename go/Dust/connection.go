package Dust

import (
	"errors"
	"io"

	"github.com/op/go-logging"

	"github.com/blanu/Dust/go/Dust/crypting"
	"github.com/blanu/Dust/go/Dust/shaping"
)

var log = logging.MustGetLogger("Dust")

var (
	ErrClosed = errors.New("Dust: connection closed")
)

// Socket provides the underlying "visible" transport for a single Dust connection.  Currently, it must be a
// stream-oriented socket, generally a TCP socket.  It must support having at least one read and one write
// simultaneously in-flight from different goroutines, and it must support being closed from any goroutine
// simultaneously with any other operations, which should cause all pending reads and writes to return as soon
// as possible.
type Socket interface {
	io.ReadWriteCloser
}

// connection holds the combination of crypting, shaping, and backend socket state for a single underlying
// Dust connection.  It can be used with different fronts to handle the different modes of the Dust stack.
type connection struct {
	socket  Socket
	crypter *crypting.Session
	front   crypting.Front
	shaper  *shaping.Shaper
	closed  bool

	shapingParams   shaping.Params
	alwaysHardClose bool

	enc ShapingEncoder
	dec ShapingDecoder

	local, remote LinkAddr
}

func (c *connection) setShapingParams(params shaping.Params) {
	c.shapingParams = params
	c.alwaysHardClose = !params.IgnoreDuration
}

func (c *connection) Close() error {
	if c.alwaysHardClose {
		return c.HardClose()
	}

	if c.closed {
		return ErrClosed
	}

	// We don't actually need to do anything here; we let the shaper continue to run in the background.
	// It'll exit when its designated interval elapses.  The crypter already has backpressure relief, so
	// it'll automatically be draining any further real data frames into the bit bucket (maybe a bit
	// inefficiently).  The shaper has responsibility for closing the socket too.
	c.closed = true
	return nil
}

func (c *connection) HardClose() error {
	if c.socket == nil {
		return ErrClosed
	}

	log.Info("hard-closing %v <-> %v", c.local, c.remote)

	// Make the shaper exit as soon as it can, and close the socket immediately.  Can't do much beyond
	// that.
	err := c.socket.Close()
	c.socket = nil
	c.shaper.CloseDetach()
	c.closed = true
	return err
}

func (c *connection) initAny(front crypting.Front) {
	c.local = genLinkAddr()
	c.remote = genLinkAddr()
	c.front = front
}

func (c *connection) initClient(spub *ServerPublic, front crypting.Front) (err error) {
	c.initAny(front)

	model, err := spub.ReifyModel()
	if err != nil {
		log.Error("initClient: retrieving model: %v", err)
		return
	}

	c.enc, c.dec, err = model.MakeClientPair()
	if err != nil {
		log.Error("initClient: constructing codec: %v", err)
		return
	}

	c.crypter, err = crypting.BeginClient(spub.cryptoPublic(), c.front, spub.cryptingParams)
	if err != nil {
		log.Error("initClient: starting crypting session: %v", err)
		return
	}

	c.setShapingParams(spub.shapingParams)
	return
}

func (c *connection) initServer(spriv *ServerPrivate, front crypting.Front) (err error) {
	c.initAny(front)

	model, err := spriv.ReifyModel()
	if err != nil {
		log.Error("initServer: retrieving model: %v", err)
		return
	}

	c.enc, c.dec, err = model.MakeServerPair()
	if err != nil {
		log.Error("initServer: constructing codec: %v", err)
		return
	}

	c.crypter, err = crypting.BeginServer(spriv.cryptoPrivate(), c.front, spriv.cryptingParams)
	if err != nil {
		log.Error("initServer: starting crypting session: %v", err)
		return
	}

	c.setShapingParams(spriv.shapingParams)
	return
}

func (c *connection) spawn(socket Socket) (err error) {
	c.socket = socket

	c.shaper, err = shaping.NewShaper(c.crypter, c.socket, c.dec, c.socket, c.enc, c.socket, c.shapingParams)
	if err != nil {
		log.Error("spawn: starting shaper: %v", err)
		return
	}
	c.shaper.Spawn()

	log.Info("started %v <-> %v", c.local, c.remote)
	return
}
