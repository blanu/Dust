// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package Dust

import (
	"errors"
	"io"
	"sync"
	"sync/atomic"

	"github.com/blanu/Dust/go/v2/crypting"
	"github.com/blanu/Dust/go/v2/shaping"
)

var (
	ErrClosed = errors.New("Dust: connection closed")
)

// Socket provides the underlying "visible" transport for a single Dust connection.  Currently, it must be a
// stream-oriented socket, generally a TCP socket.  It must support having at least one read and one write
// simultaneously in-flight from different goroutines.  It must support being closed from any goroutine
// simultaneously with any other operations, which should cause all pending reads and writes to return as soon
// as possible, and all future reads and writes to fail immediately.
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

	closed     uint32
	closeMutex sync.Mutex
	hardClose  bool

	shapingParams shaping.Params

	enc ShapingEncoder
	dec ShapingDecoder

	local, remote LinkAddr
}

func (c *connection) setShapingParams(params shaping.Params) {
	c.shapingParams = params
	c.hardClose = params.IgnoreDuration
}

func (c *connection) Close() error {
	switch {
	case atomic.LoadUint32(&c.closed) != 0:
		return ErrClosed
	case c.hardClose:
		return c.HardClose()
	default:
		// The shaper is detached and continues to run in the background.  It'll exit when its
		// designated interval elapses, and is responsible for closing the socket.  The crypter will
		// already be draining any further real data frames into the bit bucket from both sides.
		atomic.StoreUint32(&c.closed, 1)
		return nil
	}
}

func (c *connection) HardClose() error {
	c.closeMutex.Lock()
	defer c.closeMutex.Unlock()
	defer atomic.StoreUint32(&c.closed, 1)
	if c.socket == nil {
		return ErrClosed
	}

	log.Info("hard-closing %v <-> %v", c.local, c.remote)

	// Cancel the shaper process, and close the socket immediately.  Any shaper I/O on the socket should
	// then immediately fail.  Can't do much beyond that.
	err := c.socket.Close()
	c.socket = nil
	c.shaper.Cancel()
	return err
}

func (c *connection) initAny(front crypting.Front) {
	c.local = genLinkAddr()
	c.remote = genLinkAddr()
	c.front = front
}

func (c *connection) initClient(spub *ServerPublic, front crypting.Front) (err error) {
	c.initAny(front)

	model, err := spub.EndpointParams.ModelSpec.reifyModel()
	if err != nil {
		log.Error("initClient: retrieving model: %v", err)
		return
	}

	c.enc, c.dec, err = model.MakeClientPair()
	if err != nil {
		log.Error("initClient: constructing codec: %v", err)
		return
	}

	c.crypter, err = crypting.BeginClient(spub.cryptoPublic(), c.front, spub.Crypting)
	if err != nil {
		log.Error("initClient: starting crypting session: %v", err)
		return
	}

	c.setShapingParams(spub.Shaping)
	return
}

func (c *connection) initServer(spriv *ServerPrivate, front crypting.Front) (err error) {
	c.initAny(front)

	model, err := spriv.EndpointParams.ModelSpec.reifyModel()
	if err != nil {
		log.Error("initServer: retrieving model: %v", err)
		return
	}

	c.enc, c.dec, err = model.MakeServerPair()
	if err != nil {
		log.Error("initServer: constructing codec: %v", err)
		return
	}

	c.crypter, err = crypting.BeginServer(spriv.cryptoPrivate(), c.front, spriv.Crypting)
	if err != nil {
		log.Error("initServer: starting crypting session: %v", err)
		return
	}

	c.setShapingParams(spriv.Shaping)
	return
}

func (c *connection) spawn(socket Socket) (err error) {
	c.socket = socket
	var closer io.Closer = socket
	if c.hardClose {
		// We take responsibility for closing the socket.
		closer = nil
	}

	// TODO: doc why no parent env propagation here
	c.shaper, err = shaping.NewShaper(nil, c.crypter, c.socket, c.dec, c.socket, c.enc, closer, &c.shapingParams)
	if err != nil {
		log.Error("spawn: starting shaper: %v", err)
		return
	}
	c.shaper.Start()

	log.Info("started %v <-> %v", c.local, c.remote)
	return
}
