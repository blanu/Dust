/*
Package Dust implements the main Dust protocol codec for TCP/IP.

At initialization time, use RegisterModel to associate model names with model constructors.

To use this library, use LoadServerPublicBridgeLine or LoadServerPrivateFile to acquire the
public or private side of a server identifier, respectively.  For each connection desired, use BeginClient or
BeginServer to establish a new session, which you may use to exchange streams of octets.  Dust sessions will
normally continue running in the background for the duration provided by the model encoder.
*/
package Dust

import (
	"errors"
	"io"

	"github.com/blanu/Dust/go/Dust/crypting"
	"github.com/blanu/Dust/go/Dust/shaping"
)

var (
	ErrClosed = errors.New("connection closed")
)

type Connection interface {
	io.ReadWriteCloser
	HardClose() error
}

type session struct {
	socket io.ReadWriteCloser
	crypter *crypting.Session
	shaper *shaping.Shaper
	closed bool
}

func (s *session) Read(p []byte) (n int, err error) {
	if s.closed {
		return 0, ErrClosed
	}
	
	return s.crypter.Read(p)
}

func (s *session) Write(p []byte) (n int, err error) {
	if s.closed {
		return 0, ErrClosed
	}
	
	return s.crypter.Write(p)
}

func (s *session) Close() error {
	if s.closed {
		return ErrClosed
	}

	// TODO: actually support close
	//err := s.crypter.SoftClose()
	s.closed = true
	return nil
}

func (s *session) HardClose() error {
	if s.socket == nil {
		return ErrClosed
	}

	// TODO: actually support hard-close beyond this... ?
	err := s.socket.Close()
	s.socket = nil
	s.shaper.CloseDetach()
	return err
}

func BeginClient(socket io.ReadWriteCloser, spub *ServerPublic) (conn Connection, err error) {
	model, err := spub.ReifyModel()
	if err != nil {
		return
	}

	enc, dec, err := model.MakeClientPair()
	if err != nil {
		return
	}

	crypter, err := crypting.BeginClient(spub.cryptoPublic())
	if err != nil {
		return
	}

	shaper, err := shaping.NewShaper(crypter, socket, dec, socket, enc)
	if err != nil {
		return
	}

	shaper.Spawn()
	conn = &session{
		socket: socket,
		crypter: crypter,
		shaper: shaper,
	}
	return
}

func BeginServer(socket io.ReadWriteCloser, spriv *ServerPrivate) (conn Connection, err error) {
	model, err := spriv.ReifyModel()
	if err != nil {
		return
	}

	enc, dec, err := model.MakeServerPair()
	if err != nil {
		return
	}

	crypter, err := crypting.BeginServer(spriv.cryptoPrivate())
	if err != nil {
		return
	}

	shaper, err := shaping.NewShaper(crypter, socket, dec, socket, enc)
	if err != nil {
		return
	}

	shaper.Spawn()
	conn = &session{
		socket: socket,
		crypter: crypter,
		shaper: shaper,
	}
	return
}
