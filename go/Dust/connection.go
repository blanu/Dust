/*
Package Dust implements the main Dust protocol codec for TCP/IP.

At initialization time, use RegisterModel to associate model names with model constructors.

To use this library, use LoadServerPublicBridgeLine or LoadServerPrivateFile to acquire the public or private
side of a server identifier, respectively.  For each connection desired, use BeginClientConnection or
BeginServerConnection to establish a new connection, which you may use to exchange streams of datagrams.  Dust
connections will normally continue running in the background for the duration provided by the model encoder.
*/
package Dust

import (
	"errors"
	"io"

	"github.com/op/go-logging"

	"github.com/blanu/Dust/go/Dust/crypting"
	"github.com/blanu/Dust/go/Dust/shaping"
)

var (
	ErrClosed = errors.New("Dust: connection closed")
)

var log = logging.MustGetLogger("Dust")

// Connection acts mostly as a datagram-oriented I/O channel.  However, closing it does not necessarily close
// the backing channel, as Dust connections normally have a predetermined duration.  The HardClose method is
// specified if the backing channel must also be immediately closed for some reason; this should be avoided as
// it may cause the Dust connection to be more easily detectable by an intermediary.  (TODO: doc variance between
// this and a "pure" datagram-oriented I/O channel)
type Connection interface {
	io.ReadWriteCloser
	HardClose() error
}

type singleConnection struct {
	socket  io.ReadWriteCloser
	crypter *crypting.Session
	shaper  *shaping.Shaper
	closed  bool
}

func (s *singleConnection) Read(p []byte) (n int, err error) {
	if s.closed {
		return 0, ErrClosed
	}

	return s.crypter.Read(p)
}

func (s *singleConnection) Write(p []byte) (n int, err error) {
	if s.closed {
		return 0, ErrClosed
	}

	return s.crypter.Write(p)
}

func (s *singleConnection) Close() error {
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

func (s *singleConnection) HardClose() error {
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

var defCryptingParams = crypting.Params{
	MTU: 1500,
}

// BeginClientConnection initiates the client side of a Dust connection using the public key and model
// parameters specified in spub.  Dust-level communication will occur in the background over socket, which is
// usually the client side of a TCP connection and must be a stream-oriented I/O channel.  The Dust connection
// takes over responsibility for closing socket.
func BeginClientConnection(socket io.ReadWriteCloser, spub *ServerPublic) (conn Connection, err error) {
	model, err := spub.ReifyModel()
	if err != nil {
		log.Error("BeginClient: retrieving model: %v", err)
		return
	}

	enc, dec, err := model.MakeClientPair()
	if err != nil {
		log.Error("BeginClient: constructing codec: %v", err)
		return
	}

	crypter, err := crypting.BeginClient(spub.cryptoPublic(), spub.cryptingParams)
	if err != nil {
		log.Error("BeginClient: starting crypting session: %v", err)
		return
	}

	shaper, err := shaping.NewShaper(crypter, socket, dec, socket, enc, socket)
	if err != nil {
		log.Error("BeginClient: starting shaper: %v", err)
		return
	}

	shaper.Spawn()
	conn = &singleConnection{
		socket:  socket,
		crypter: crypter,
		shaper:  shaper,
	}
	return
}

// BeginServerConnection initiates the server side of a Dust connection using the private key and model
// parameters specified in spriv.  Dust-level communication will occur in the background over socket, which is
// usually an accepted TCP connection and must be a stream-oriented I/O channel.  The Dust connection takes
// over responsibility for closing socket.
func BeginServerConnection(socket io.ReadWriteCloser, spriv *ServerPrivate) (conn Connection, err error) {
	model, err := spriv.ReifyModel()
	if err != nil {
		log.Error("BeginServer: retrieving model: %v", err)
		return
	}

	enc, dec, err := model.MakeServerPair()
	if err != nil {
		log.Error("BeginServer: constructing codec: %v", err)
		return
	}

	crypter, err := crypting.BeginServer(spriv.cryptoPrivate(), spriv.cryptingParams)
	if err != nil {
		log.Error("BeginServer: starting crypting session: %v", err)
		return
	}

	shaper, err := shaping.NewShaper(crypter, socket, dec, socket, enc, socket)
	if err != nil {
		log.Error("BeginServer: starting shaper: %v", err)
		return
	}

	shaper.LogError = func(e error) {
		// TODO: filter other errors
		if e == io.EOF {
			return
		}
		log.Error("shaper exited with %v", e)
	}

	shaper.Spawn()
	conn = &singleConnection{
		socket:  socket,
		crypter: crypter,
		shaper:  shaper,
	}
	return
}
