// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package crypting

import (
	"errors"

	"github.com/op/go-logging"

	"github.com/blanu/Dust/go/buf"
	"github.com/blanu/Dust/go/prim1"
)

var log = logging.MustGetLogger("Dust/crypting")

var (
	ErrBadHandshake     = errors.New("Dust/crypting: bad handshake")
	ErrBadDecode        = errors.New("Dust/crypting: bad decode")
	ErrDatagramTooLarge = errors.New("Dust/crypting: datagram too large")
)

const (
	frameOverhead = 2 + 32

	kdfC2S = `cli.`
	kdfS2C = `srv.`

	kdfCipherData = `dta.`
	kdfMACData    = `grm.`
	kdfMACConfirm = `bgn.`
)

type state int

const (
	stateFailed state = iota
	stateHandshakeNoKey
	stateHandshakeKey
	stateStreaming
)

// A Session holds state for a single secure channel.  There are two "sides" to a session: the outward-facing
// side and the inward-facing side.  The outward-facing side transceives arbitrary-rate streams of uniform
// bytes; the inward-facing side transceives a plaintext application protocol to be run over the secure
// channel.  The outward-facing side is accessed by the shaping layer via PullWrite and PushRead; concurrently,
// the inward-facing side is accessed by the application via Write and Read.  The two sides communicate using
// channels, but no more than one goroutine should access a side at a time.
type Session struct {
	Params

	front Front

	pushBuffer [][]byte

	// Failed: we're using a bogus key to send, and received bytes are discarded.  HandshakeNoKey: we're
	// reassembling the remote side's ephemeral public key.  HandshakeKey: we're searching for the
	// confirmation code from the remote side.  Streaming: framed bytes can be sent and received.
	state state

	// This is a *Public for clients and *Private for servers.
	serverInfo interface{}

	// These both correspond to ephemeral keys.  The server's long-term key or keypair will be
	// stashed somewhere in serverInfo.  localPrivate is always set.  remotePublic is set if state is
	// HandshakeKey.
	localPrivate prim.Private
	remotePublic prim.Public

	// Incoming data must be processed against this first, if set, before inCipher.  When HandshakeNoKey:
	// reassembling an ephemeral public key.  When HandshakeKey: searching for the confirmation code from
	// the remote side.
	inHandshake buf.Reassembly

	// Set in HandshakeKey state.
	inConfirmation prim.AuthValue

	// Set in Streaming state.  In other states, inCipher uses a random key.  inPosition refers to the
	// position of the MAC for incoming data.
	inCipher   prim.Cipher
	inMAC      prim.VerifyingMAC
	inPosition uint64

	// Data is reassembled into frames here.  The reassembly capacity is the maximum frame wire size.  The
	// earliest byte of inStreaming is at inFrameStart of the stream, and this is measured against
	// inPosition to determine how far the MAC has been advanced.
	inStreaming  buf.Reassembly
	inFrameStart uint64

	// Implicitly prepended to any other outgoing data; these bytes are not encrypted before sending.
	outCrypted buf.Reassembly

	// Set in Streaming state.  In other states, outCipher uses a random key.
	outCipher   prim.Cipher
	outMAC      prim.GeneratingMAC
	outPosition uint64

	// Used for advancing the stream cipher when copying MAC bytes; must be exactly one MAC-length long.
	outGarbage []byte
}

// Move from any state to the Failed state, setting a bogus session key and destroying any in-progress data.
func (cs *Session) fail() {
	log.Info("session failure")
	cs.state = stateFailed
	cs.inHandshake = nil
	cs.inStreaming = nil
	cs.outCipher.SetRandomKey()
	cs.outCrypted = nil
}

func (cs *Session) Init() error {
	if err := cs.Params.Validate(); err != nil {
		return err
	}

	if !(MinMTU <= cs.MTU && cs.MTU <= MaxMTU) {
		return ErrBadMTU
	}

	cs.localPrivate = prim.NewPrivate()
	cs.inCipher.SetRandomKey()
	cs.outCipher.SetRandomKey()
	cs.outCrypted = buf.BeginReassembly(cs.MTU + 32 + frameOverhead)
	cs.outGarbage = make([]byte, 32)
	startingData := cs.localPrivate.Public.Binary()
	buf.CopyReassemble(&cs.outCrypted, &startingData)
	cs.inHandshake = buf.BeginReassembly(32)
	cs.state = stateHandshakeNoKey
	return nil
}

func beginAny(sinfo interface{}, front Front, params Params) (*Session, error) {
	cs := &Session{
		Params:     params,
		front:      front,
		serverInfo: sinfo,
	}
	if err := cs.Init(); err != nil {
		return nil, err
	}

	return cs, nil
}

// BeginClient starts a new crypting session from the client's perspective, given a server's public
// cryptographic parameters.
func BeginClient(pub *Public, front Front, params Params) (*Session, error) {
	return beginAny(pub, front, params)
}

// BeginServer starts a new crypting session from the server's perspective, given the server's private
// cryptographic parameters.
func BeginServer(priv *Private, front Front, params Params) (*Session, error) {
	return beginAny(priv, front, params)
}
