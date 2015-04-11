// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package Dust

import (
	"io"
	"net"

	"github.com/blanu/Dust/go/v2/crypting"
)

type RawParams struct {
}

type DustPacketConn interface {
	io.ReadWriter
	ReadFrom(p []byte) (n int, addr net.Addr, err error)
	ReadFromDust(p []byte) (n int, addr LinkAddr, err error)
	WriteTo(p []byte, addr net.Addr) (n int, err error)
	WriteToDust(p []byte, addr LinkAddr) (n int, err error)
	Close() error
	HardClose() error
}

type RawConn struct {
	connection
	RawParams
	invFront *crypting.InvertingFront
}

var _ DustPacketConn = (*RawConn)(nil)

func (rconn *RawConn) LocalAddr() net.Addr {
	return &rconn.local
}

func (rconn *RawConn) LocalAddrDust() LinkAddr {
	return rconn.local
}

func (rconn *RawConn) RemoteAddr() net.Addr {
	return &rconn.remote
}

func (rconn *RawConn) RemoteAddrDust() LinkAddr {
	return rconn.remote
}

func (rconn *RawConn) Read(p []byte) (n int, err error) {
	return rconn.invFront.Read(p)
}

func (rconn *RawConn) ReadFrom(p []byte) (n int, addr net.Addr, err error) {
	n, err = rconn.Read(p)
	addr = &rconn.remote
	return
}

func (rconn *RawConn) ReadFromDust(p []byte) (n int, addr LinkAddr, err error) {
	n, err = rconn.Read(p)
	addr = rconn.remote
	return
}

func (rconn *RawConn) Write(p []byte) (n int, err error) {
	return rconn.invFront.Write(p)
}

func (rconn *RawConn) WriteTo(p []byte, addr net.Addr) (n int, err error) {
	switch a := addr.(type) {
	case nil:
		// Zero address is okay.
	case *LinkAddr:
		if !LinkAddrEqual(*a, rconn.remote) {
			err = ErrUnreachable
			return
		}
	default:
		err = ErrAfNoSupport
		return
	}

	n, err = rconn.Write(p)
	return
}

func (rconn *RawConn) WriteToDust(p []byte, addr LinkAddr) (n int, err error) {
	if !LinkAddrEqual(addr, rconn.remote) {
		err = ErrUnreachable
		return
	}

	n, err = rconn.Write(p)
	return
}

func (rconn *RawConn) Close() error {
	return rconn.connection.Close()
}

func (rconn *RawConn) HardClose() error {
	return rconn.connection.HardClose()
}

func (rconn *RawConn) MTU() int {
	return rconn.connection.crypter.MTU
}

func newRawConn(params *RawParams, cryptingParams *crypting.Params) (rconn *RawConn) {
	rconn = &RawConn{}
	if params != nil {
		rconn.RawParams = *params
	}
	rconn.invFront = crypting.NewInvertingFront(*cryptingParams)
	return
}

func BeginRawClient(socket Socket, spub *ServerPublic, params *RawParams) (rconn *RawConn, err error) {
	rconn = newRawConn(params, &spub.Crypting)
	if err = rconn.initClient(spub, rconn.invFront); err != nil {
		return
	}
	if err = rconn.spawn(socket); err != nil {
		return
	}
	return
}

func BeginRawServer(socket Socket, spriv *ServerPrivate, params *RawParams) (rconn *RawConn, err error) {
	rconn = newRawConn(params, &spriv.Crypting)
	if err = rconn.initServer(spriv, rconn.invFront); err != nil {
		return
	}
	if err = rconn.spawn(socket); err != nil {
		return
	}
	return
}
