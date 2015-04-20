// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package crypting

import (
	"io"
	"time"

	"github.com/op/go-logging"

	"github.com/blanu/Dust/go/prim1"
)

var log = logging.MustGetLogger("Dust/crypting")

const (
	frameOverhead = 2 + prim.AuthLen
)

type state int

const (
	// Using a bogus key to send continuous padding.  Received bytes are discarded.
	stateFailed state = iota

	// Reassembling the remote side's ephemeral public key.
	stateHandshakeNoKey

	// Searching for the confirmation code from the remote side.
	stateHandshakeKey

	// Channel established.  Framed datagrams can be sent and received.
	stateEstablished
)

type InterruptibleReadWriter interface {
	io.ReadWriter
	SetInterrupt(ch <-chan struct{}) error
	SetReadInterrupt(ch <-chan struct{}) error
	SetWriteInterrupt(ch <-chan struct{}) error
}

type DeadlineReadWriter interface {
	io.ReadWriter
	SetDeadline(t time.Time) error
	SetReadDeadline(t time.Time) error
	SetWriteDeadline(t time.Time) error
}

// A Session holds state for a single secure channel.  There are two "sides" to a session: the outward-facing
// side and the inward-facing side.  The outward-facing side transceives arbitrary-rate streams of uniform
// bytes; the inward-facing side transceives a plaintext application protocol to be run over the secure
// channel.  The outward-facing side is accessed by the shaping layer via PullWrite and PushRead; concurrently,
// the inward-facing side is accessed by the application via Write and Read.  The two sides communicate using
// channels, but no more than one goroutine should access a side at a time.
type Session struct {
	Params

	Front DeadlineReadWriter
	Back  InterruptibleReadWriter

	// Shared handshake state.  The incoming side manipulates this as it receives handshake data, and
	// sends messages to the outgoing side about what to output.
	handshake handshake

	// Channel used to propagate advancement of handshake state from incoming to outgoing side.
	stateChan chan state

	// Each stream manages its own state; they may be accessed independently.
	incoming incoming
	outgoing outgoing

	// Server identity: a *Public for clients, or a *Private for servers.
	serverInfo interface{}
}

func (cs *Session) Init(sinfo interface{}) error {
	if err := cs.Params.Validate(); err != nil {
		return err
	}

	cs.stateChan = make(chan state, 3)
	greeting := cs.handshake.start(sinfo)
	cs.incoming.Init(cs)
	cs.outgoing.Init(cs, greeting)
	cs.Front = &ioPair{rd: &cs.incoming, wr: &cs.outgoing}
	cs.Back = &ioPair{rd: &cs.outgoing, wr: &cs.incoming}
	return nil
}

func beginAny(sinfo interface{}, params Params) (*Session, error) {
	cs := &Session{Params: params}
	if err := cs.Init(sinfo); err != nil {
		return nil, err
	}

	return cs, nil
}

// BeginClient starts a new crypting session from the client's perspective, given a server's public
// cryptographic parameters.
func BeginClient(pub *Public, params Params) (*Session, error) {
	return beginAny(pub, params)
}

// BeginServer starts a new crypting session from the server's perspective, given the server's private
// cryptographic parameters.
func BeginServer(priv *Private, params Params) (*Session, error) {
	return beginAny(priv, params)
}
