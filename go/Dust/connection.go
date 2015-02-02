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
	ErrClosed = errors.New("Dust: connection closed")
)

// Connection acts as a stream-oriented I/O channel.  However, closing it does not necessarily close the
// backing channel, as Dust connections normally have a predetermined duration.  The HardClose method is
// specified if the backing channel must also be immediately closed for some reason; this should be avoided
// as it may cause the Dust connection to be more easily detectable by an intermediary.
type Connection interface {
	io.ReadWriteCloser
	HardClose() error
}

type session struct {
	socket  io.ReadWriteCloser
	crypter *crypting.Session
	shaper  *shaping.Shaper
	closed  bool
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

	// We don't actually need to do anything here; we let the shaper continue to run in the background.
	// It'll exit when its designated interval elapses.  The crypter already has backpressure relief, so
	// it'll automatically be draining any further real data frames into the bit bucket (maybe a bit
	// inefficiently).  The shaper has responsibility for closing the socket too.
	s.closed = true
	return nil
}

func (s *session) HardClose() error {
	if s.socket == nil {
		return ErrClosed
	}

	// Make the shaper exit as soon as it can, and close the socket immediately.  Can't do much beyond
	// that.
	err := s.socket.Close()
	s.socket = nil
	s.shaper.CloseDetach()
	s.closed = true
	return err
}

// BeginClient initiates the client side of a Dust connection using the public key and model parameters
// specified in spub.  Dust-level communication will occur in the background over socket, which is usually the
// client side of a TCP connection and must be a stream-oriented I/O channel.  The Dust connection takes over
// responsibility for closing socket.
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

	shaper, err := shaping.NewShaper(crypter, socket, dec, socket, enc, socket)
	if err != nil {
		return
	}

	shaper.Spawn()
	conn = &session{
		socket:  socket,
		crypter: crypter,
		shaper:  shaper,
	}
	return
}

// BeginServer initiates the server side of a Dust connection using the private key and model parameters
// specified in spriv.  Dust-level communication will occur in the background over socket, which is usually
// an accepted TCP connection and must be a stream-oriented I/O channel.  The Dust connection takes over
// responsibility for closing socket.
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

	shaper, err := shaping.NewShaper(crypter, socket, dec, socket, enc, socket)
	if err != nil {
		return
	}

	shaper.Spawn()
	conn = &session{
		socket:  socket,
		crypter: crypter,
		shaper:  shaper,
	}
	return
}
