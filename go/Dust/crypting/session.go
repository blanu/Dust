package crypting

import (
	"errors"

	"github.com/op/go-logging"

	"github.com/blanu/Dust/go/Dust/buf"
	"github.com/blanu/Dust/go/Dust/prim"
)

var log = logging.MustGetLogger("Dust/crypting")

var (
	ErrBadHandshake      = errors.New("Dust/crypting: bad handshake")
	ErrBadDecode         = errors.New("Dust/crypting: bad decode")
	ErrDatagramTooLarge  = errors.New("Dust/crypting: datagram too large")
	ErrSomeDatagramsLost = errors.New("Dust/crypting: some datagrams lost")
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

type numberedGram struct {
	seq  int64
	data []byte
}

// A Session holds state for a single secure channel.  There are two "sides" to a session: the outward-facing
// side and the inward-facing side.  The outward-facing side transceives arbitrary-rate streams of uniform
// bytes; the inward-facing side transceives a plaintext application protocol to be run over the secure
// channel.  The outward-facing side is accessed by the shaping layer via PullWrite and PushRead; concurrently,
// the inward-facing side is accessed by the application via Write and Read.  The two sides communicate using
// channels, but no more than one goroutine should access a side at a time.
type Session struct {
	Params

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

	// Set in Streaming state.  In other states, inCipher uses a random key.
	inCipher   prim.Cipher
	inMAC      prim.VerifyingMAC
	inPosition uint64

	// Data is reassembled into frames here.  The reassembly capacity is the maximum frame wire size.
	inStreaming buf.Reassembly

	// Datagrams with sequence numbers are sent to inPlains from the outward-facing side of the crypto
	// session.  The receiver owns the data chunks.  inSequence is the next sequence number to send from
	// the outward-facing side.  inLast is the last sequence number successfully delivered by the
	// inward-facing side, and inGramLeft buffers up to one datagram on the inward-facing side.  Sequence
	// numbers for actual datagrams start at 1.
	inGrams    chan numberedGram
	inSequence int64
	inLast     int64
	inLossage  int64
	inGramLeft numberedGram

	// Implicitly prepended to any other outgoing data; these bytes are not encrypted before sending.
	outCrypted buf.Reassembly

	// Set in Streaming state.  In other states, outCipher uses a random key.
	outCipher   prim.Cipher
	outMAC      prim.GeneratingMAC
	outPosition uint64

	// Datagrams are sent to outPlains from the inward-facing side of the crypto session.  The receiver
	// owns the chunks.
	outPlains chan []byte
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

func (cs *Session) Init(sinfo interface{}) error {
	if err := ValidateParams(cs.Params); err != nil {
		return err
	}

	if !(MinMTU <= cs.MTU && cs.MTU <= MaxMTU) {
		return ErrBadMTU
	}

	cs.localPrivate = prim.NewPrivate()
	cs.serverInfo = sinfo
	cs.inGrams = make(chan numberedGram, 4)
	cs.outPlains = make(chan []byte, 4)
	cs.inCipher.SetRandomKey()
	cs.outCipher.SetRandomKey()
	cs.outCrypted = buf.BeginReassembly(cs.MTU + 32 + frameOverhead)
	startingData := cs.localPrivate.Public.Binary()
	buf.CopyReassemble(&cs.outCrypted, &startingData)
	cs.inHandshake = buf.BeginReassembly(32)
	cs.state = stateHandshakeNoKey
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
