package Dust

import (
	"io"
	"net"

	"github.com/blanu/Dust/go/Dust/crypting"
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

func (rc *RawConn) LocalAddr() net.Addr {
	return &rc.local
}

func (rc *RawConn) LocalAddrDust() LinkAddr {
	return rc.local
}

func (rc *RawConn) RemoteAddr() net.Addr {
	return &rc.remote
}

func (rc *RawConn) RemoteAddrDust() LinkAddr {
	return rc.remote
}

func (rc *RawConn) Read(p []byte) (n int, err error) {
	if rc.closed {
		err = ErrClosed
		return
	}

	return rc.invFront.Read(p)
}

func (rc *RawConn) ReadFrom(p []byte) (n int, addr net.Addr, err error) {
	n, err = rc.Read(p)
	addr = &rc.remote
	return
}

func (rc *RawConn) ReadFromDust(p []byte) (n int, addr LinkAddr, err error) {
	n, err = rc.Read(p)
	addr = rc.remote
	return
}

func (rc *RawConn) Write(p []byte) (n int, err error) {
	if rc.closed {
		err = ErrClosed
		return
	}

	return rc.invFront.Write(p)
}

func (rc *RawConn) WriteTo(p []byte, addr net.Addr) (n int, err error) {
	switch a := addr.(type) {
	case nil:
		// Zero address is okay.
	case *LinkAddr:
		if !LinkAddrEqual(*a, rc.remote) {
			err = ErrUnreachable
			return
		}
	default:
		err = ErrAfNoSupport
		return
	}

	n, err = rc.Write(p)
	return
}

func (rc *RawConn) WriteToDust(p []byte, addr LinkAddr) (n int, err error) {
	if !LinkAddrEqual(addr, rc.remote) {
		err = ErrUnreachable
		return
	}

	n, err = rc.Write(p)
	return
}

func (rc *RawConn) Close() error {
	return rc.connection.Close()
}

func (rc *RawConn) HardClose() error {
	return rc.connection.HardClose()
}

func (rc *RawConn) MTU() int {
	return rc.connection.crypter.MTU
}

func BeginRawClient(socket Socket, spub *ServerPublic, params RawParams) (conn *RawConn, err error) {
	conn = &RawConn{RawParams: params}
	conn.invFront = crypting.NewInvertingFront(spub.cryptingParams)
	if err = conn.initClient(spub, conn.invFront); err != nil {
		return
	}
	if err = conn.spawn(socket); err != nil {
		return
	}
	return
}

func BeginRawServer(socket Socket, spriv *ServerPrivate, params RawParams) (conn *RawConn, err error) {
	conn = &RawConn{RawParams: params}
	conn.invFront = crypting.NewInvertingFront(spriv.cryptingParams)
	if err = conn.initServer(spriv, conn.invFront); err != nil {
		return
	}
	if err = conn.spawn(socket); err != nil {
		return
	}
	return
}
