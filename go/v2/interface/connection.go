// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package Dust

import (
	"errors"
	"io"
	"sync"

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
// Dust connection.
type connection struct {
	socket  Socket
	crypter *crypting.Session
	shaper  *shaping.Shaper

	closeOnce  sync.Once
	closeError error

	enc ShapingEncoder
	dec ShapingDecoder

	local, remote LinkAddr
}

func (c *connection) hardClose() {
	// Cancel the shaper process, and close the socket immediately.  Any shaper I/O on the socket should
	// then immediately fail.  Can't do much beyond that.
	err := c.socket.Close()
	c.socket = nil
	c.shaper.Cancel()
	log.Info("closing %v <-> %v", c.local, c.remote)
	c.closeError = err
}

func (c *connection) Close() error {
	c.closeOnce.Do(c.hardClose)
	return c.closeError
}

func (c *connection) initAny() {
	c.local = genLinkAddr()
	c.remote = genLinkAddr()
}

func (c *connection) initClient(spub *ServerPublic) (err error) {
	c.initAny()

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

	c.crypter, err = crypting.BeginClient(spub.cryptoPublic(), spub.Crypting)
	if err != nil {
		log.Error("initClient: starting crypting session: %v", err)
		return
	}

	return
}

func (c *connection) initServer(spriv *ServerPrivate) (err error) {
	c.initAny()

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

	c.crypter, err = crypting.BeginServer(spriv.cryptoPrivate(), spriv.Crypting)
	if err != nil {
		log.Error("initServer: starting crypting session: %v", err)
		return
	}

	return
}

func (c *connection) spawn(socket Socket) (err error) {
	c.socket = socket

	// TODO: doc why no parent env propagation here
	c.shaper, err = shaping.NewShaper(nil, c.crypter, c.socket, c.dec, c.socket, c.enc)
	if err != nil {
		log.Error("spawn: starting shaper: %v", err)
		return
	}
	c.shaper.Start()

	log.Info("started %v <-> %v", c.local, c.remote)
	return
}
