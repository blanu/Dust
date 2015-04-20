// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package Dust

import (
	"errors"
	"net"
	"time"
)

var (
	// TODO: support deadlines, of course.
	ErrDeadlineNotSupported = errors.New("Dust: I/O deadline not supported")
)

type RawStreamConn struct {
	connection
}

var _ net.Conn = (*RawStreamConn)(nil)

func (rconn *RawStreamConn) LocalAddr() net.Addr {
	return &rconn.local
}

func (rconn *RawStreamConn) RemoteAddr() net.Addr {
	return &rconn.remote
}

func (rconn *RawStreamConn) Read(p []byte) (n int, err error) {
	return rconn.crypter.Front.Read(p)
}

func (rconn *RawStreamConn) Write(p []byte) (n int, err error) {
	return rconn.crypter.Front.Write(p)
}

func (rconn *RawStreamConn) Close() error {
	return rconn.connection.Close()
}

func (rconn *RawStreamConn) SetDeadline(t time.Time) error {
	return rconn.crypter.Front.SetDeadline(t)
}

func (rconn *RawStreamConn) SetReadDeadline(t time.Time) error {
	return rconn.crypter.Front.SetReadDeadline(t)
}

func (rconn *RawStreamConn) SetWriteDeadline(t time.Time) error {
	return rconn.crypter.Front.SetWriteDeadline(t)
}

func BeginRawStreamClient(socket Socket, spub *ServerPublic) (rconn *RawStreamConn, err error) {
	rconn = &RawStreamConn{}
	if err = rconn.initClient(spub); err != nil {
		return
	}
	if err = rconn.spawn(socket); err != nil {
		return
	}
	return
}

func BeginRawStreamServer(socket Socket, spriv *ServerPrivate) (rconn *RawStreamConn, err error) {
	rconn = &RawStreamConn{}
	if err = rconn.initServer(spriv); err != nil {
		return
	}
	if err = rconn.spawn(socket); err != nil {
		return
	}
	return
}
