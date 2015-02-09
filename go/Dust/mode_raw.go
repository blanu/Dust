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
	if rconn.closed {
		err = ErrClosed
		return
	}

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
	if rconn.closed {
		err = ErrClosed
		return
	}

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
