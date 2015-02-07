package crypting

import (
	"bytes"
	"errors"
	"io"

	"github.com/op/go-logging"

	"github.com/blanu/Dust/go/Dust/buf"
	"github.com/blanu/Dust/go/Dust/prim"
)

var log = logging.MustGetLogger("Dust/crypting")

var (
	ErrIncompleteFrame = errors.New("Dust/crypting: incomplete frame")
	ErrBadHandshake    = errors.New("Dust/crypting: bad handshake")
	ErrBadDecode       = errors.New("Dust/crypting: bad decode")
	ErrStuck           = errors.New("Dust/crypting: cannot get anything to send here")
)

const (
	kdfC2S = `cli.`
	kdfS2C = `srv.`

	kdfCipherData = `dta.`
	kdfMACData    = `grm.`
	kdfMACConfirm = `bgn.`
)

const (
	maxInFrameDataSize  = 65535
	maxOutFrameDataSize = 127
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
	handshakeReassembly buf.Reassembly

	// Set in HandshakeKey state.
	inConfirmation prim.AuthValue

	// Set in Streaming state.
	inCipher   prim.Cipher
	inMAC      prim.VerifyingMAC
	inPosition uint64

	// Decrypted data is reassembled into frames here.  The reassembly capacity is the maximum frame
	// wire size.
	dataReassembly buf.Reassembly

	// Stream-oriented byte chunks are sent to inPlains from the outward-facing side of the crypto
	// session.  The receiver owns the chunks.  inPlainHeld is controlled by the receiver methods from
	// the inward-facing side, and is implicitly prepended to any receive from inPlains.
	inPlainHeld []byte
	inPlains    chan []byte

	// Implicitly prepended to any other outgoing data; these bytes are not encrypted before sending.
	outCryptPending []byte

	// Set in Streaming state, except outCipher is always set with a random key.
	outCipher   prim.Cipher
	outMAC      prim.GeneratingMAC
	outPosition uint64

	// Plaintext frames are sent to outFrames from the inward-facing side of the crypto session.  The
	// receiver owns the chunks.  Space is allocated for an authenticator, but it is not computed yet, and
	// nor is the frame encrypted.
	outFrames chan frame
}

// TODO: inPosition, outPosition not used yet

// PullWrite tries to pull post-encryption bytes into p, using in-band framing to intersperse any plain data
// available to write with enough padding frames to completely fill p immediately.  In some handshake states,
// err may be set to ErrStuck with n < len(p) to indicate that we actually can't send anything more,
// which is kind of terrible.  Note that we may still have n > 0 in that case.  This method must be called
// from the outward-facing side of the Session.  The signature is similar to that of io.Reader.Read.
func (cs *Session) PullWrite(p []byte) (n int, err error) {
	log.Debug("-> pulling %d bytes", len(p))

	n, err = 0, nil
	for len(p) > 0 {
		if len(cs.outCryptPending) > 0 {
			n += buf.CopyAdvance(&p, &cs.outCryptPending)
			continue
		}

		var frame frame

		switch cs.state {
		default:
			panic("Dust/crypting: unhandled state!")

		case stateStreaming:
			select {
			default:
				frame = newPaddingFrame(len(p))
			case frame = <-cs.outFrames:
			}

		case stateHandshakeKey:
			// TODO: this avoids sending data until we have both confirmations.  Is that the correct
			// approach?
			frame = newPaddingFrame(len(p))

		case stateFailed:
			// TODO: do we have to drain cs.outFrames here?
			frame = newPaddingFrame(len(p))

		case stateHandshakeNoKey:
			// Stream aligned 32-byte chunks from the randomly-keyed stream cipher.
			for i, _ := range p {
				p[i] = 0
			}
			cs.outCipher.XORKeyStream(p, p)

			var leftover []byte
			if len(p)%32 != 0 {
				leftover = make([]byte, 32-len(p)%32)
				cs.outCipher.XORKeyStream(leftover, leftover)
				cs.outCryptPending = append(cs.outCryptPending, leftover...)
			}

			log.Debug("-> handshake interstitial %d bytes", len(p) + len(leftover))
			n += len(p)
			return n, err
		}

		if len(frame) < 32 {
			panic("Dust/crypting: frame too small?!")
		}

		// We own frame forever now, and outCipher is valid.  Authenticate and encrypt in-place.
		cs.outMAC.Reset(0)
		frame.authenticateWith(&cs.outMAC)
		frame.encryptWith(&cs.outCipher)
		remaining := frame.slice()
		n += buf.CopyAdvance(&p, &remaining)
		if len(remaining) > 0 {
			cs.outCryptPending = append(cs.outCryptPending, remaining...)
		}
	}

	return n, err
}

// Move from any state to the Failed state, setting a bogus session key and destroying any in-progress data.
func (cs *Session) fail() {
	log.Info("session failure")
	cs.state = stateFailed
	cs.handshakeReassembly = nil
	cs.dataReassembly = nil
	cs.outCipher.SetRandomKey()
	cs.outCryptPending = nil
}

func (cs *Session) receivedEphemeralKey() {
	var err error

	received := cs.handshakeReassembly.Data()
	if len(received) != 32 {
		panic("Dust/crypting: should not have gotten here without 32-byte handshake")
	}

	cs.remotePublic, err = prim.LoadPublicBinary(received)
	if err != nil {
		panic("Dust/crypting: should always be able to load public key here")
	}

	log.Debug("  <- received ephemeral key")

	var sd prim.SecretDigest
	sd.Init()
	sd.WriteSecret(cs.localPrivate.SharedSecret(cs.remotePublic))
	var inKdfPrefix, outKdfPrefix string
	var confInput []byte

	switch sinfo := cs.serverInfo.(type) {
	default:
		panic("Dust/crypting: bad serverInfo type")

	case *Public:
		inKdfPrefix = kdfS2C
		outKdfPrefix = kdfC2S
		sd.WriteSecret(cs.localPrivate.SharedSecret(sinfo.Key))
		confInput = bytes.Join([][]byte{
			sinfo.Id,
			sinfo.Key.Binary(),
			cs.localPrivate.Public.Binary(),
			cs.remotePublic.Binary(),
		}, []byte{})

	case *Private:
		inKdfPrefix = kdfC2S
		outKdfPrefix = kdfS2C
		sd.WriteSecret(sinfo.Key.SharedSecret(cs.remotePublic))
		confInput = bytes.Join([][]byte{
			sinfo.Id,
			sinfo.Key.Public.Binary(),
			cs.remotePublic.Binary(),
			cs.localPrivate.Public.Binary(),
		}, []byte{})
	}

	sd.Write(confInput)
	secret := sd.Finish()

	outConfirmation := prim.GenerateMAC(confInput, 0, secret.DeriveAuthKey(outKdfPrefix+kdfMACConfirm))
	cs.outCryptPending = append(cs.outCryptPending, outConfirmation[:]...)
	log.Debug("-> send confirmation starting with %v", outConfirmation[:2])
	cs.outCipher.SetKey(secret.DeriveCipherKey(outKdfPrefix+kdfCipherData))
	cs.outMAC.SetKey(secret.DeriveAuthKey(outKdfPrefix+kdfMACData))
	cs.inConfirmation = prim.GenerateMAC(confInput, 0, secret.DeriveAuthKey(inKdfPrefix+kdfMACConfirm))
	log.Debug("  <- expect confirmation starting with %v", cs.inConfirmation[:2])
	cs.inCipher.SetKey(secret.DeriveCipherKey(inKdfPrefix+kdfCipherData))
	cs.inMAC.SetKey(secret.DeriveAuthKey(inKdfPrefix+kdfMACData))

	cs.handshakeReassembly = buf.BeginReassembly(32)
	cs.state = stateHandshakeKey
}

// Change to the Streaming state if we've reassembled a correct confirmation code.  Otherwise, throw away
// the 32-byte chunk and continue.
func (cs *Session) checkConfirmation() {
	if !cs.inConfirmation.Equal(cs.handshakeReassembly.Data()) {
		//log.Debug("<- discarding not-a-confirmation")
		cs.handshakeReassembly.Reset()
		return
	}

	log.Debug("  <- entering streaming state")
	// We already set up all the derived keys when we received the ephemeral key.
	cs.handshakeReassembly = nil
	cs.dataReassembly = buf.BeginReassembly(maxInFrameDataSize + 35)
	cs.state = stateStreaming
	return
}

// Attempt to decode exactly one plaintext frame and ship it out to the inward-facing side.
func (cs *Session) decodeAndShipoutPlainFrame(p []byte) (consumed int, err error) {
	// TODO: handle incoming MTU things
	frame := incomingCryptoFrame(p)
	switch {
	case frame == nil:
		return 0, ErrIncompleteFrame
	case !frame.wellFormed():
		return 0, ErrBadDecode
	}

	// TIMING: Always check the authenticator, to minimize timing surface... at least unless the Go
	// compiler decides to sink the VerifyAuthenticator down one of the branches.  Aaargh.
	//
	// TODO: fail entire stream if verification of any frame fails?  Not right now, because the spec says
	// authenticators for padding frames are allowed to be blank.
	cs.inMAC.Reset(0)
	authenticated := frame.verifyAuthenticator(&cs.inMAC)
	dataCarrying := frame.hasData()
	log.Debug("  <- frame of wire size %d, auth %v, data %v", frame.wireSize(), authenticated, dataCarrying)
	if dataCarrying && authenticated {
		// If there isn't enough buffer space in the channel, just drop the frame.  We cannot afford
		// to apply backpressure, as that would break the model.
		select {
		case cs.inPlains <- buf.CopyNew(frame.data()):
			log.Debug("  <- delivered")
		default:
			log.Debug("  <- dropped (no space)")
		}
	}

	return frame.wireSize(), nil
}

// PushRead processes new pre-decryption bytes in p, handling handshake completions and sending any usable
// plaintext that results to the inward-facing side of the Session.  This method must be called from the
// outward-facing side of the Session.  The signature is similar to io.Writer.Write.
func (cs *Session) PushRead(p []byte) (n int, err error) {
	log.Debug("  <- pushing %d bytes", len(p))
	n, err = 0, nil

	for len(p) > 0 {
		// TIMING: arguably this leads to a timing attack against whether a handshake succeeded if the
		// attacker can measure our CPU usage, but that's probably not preventable anyway?  Come back to this
		// later.
		if cs.state == stateFailed {
			return n + len(p), err
		}

		if cs.handshakeReassembly != nil {
			n += buf.CopyReassemble(&cs.handshakeReassembly, &p)
			if cs.handshakeReassembly.FixedSizeComplete() {
				switch cs.state {
				default:
					panic("Dust/crypting: handshake reassembly in weird state")
				case stateHandshakeNoKey:
					cs.receivedEphemeralKey()
				case stateHandshakeKey:
					cs.checkConfirmation()
				}
			}
		} else {
			n += buf.TransformReassemble(&cs.dataReassembly, &p, cs.inCipher.XORKeyStream)

			possibleFrames := cs.dataReassembly.Data()
		Frames:
			for {
				consumed, err := cs.decodeAndShipoutPlainFrame(possibleFrames)
				switch {
				case err == nil:
					possibleFrames = possibleFrames[consumed:]
				case err == ErrIncompleteFrame:
					err = nil
					fallthrough
				default:
					break Frames
				}
			}

			cs.dataReassembly.Consume(cs.dataReassembly.ValidLen() - len(possibleFrames))
			if err != nil {
				cs.fail()
				return n, err
			}
		}
	}

	if len(p) > 0 && err == nil {
		// Oops.
		err = io.ErrShortWrite
	}
	return n, err
}

// Read reads as much new plaintext data as is available from cs, blocking if and only if none would otherwise
// be available.  The signature is that of io.Reader.Read.
func (cs *Session) Read(p []byte) (n int, err error) {
	if log.IsEnabledFor(logging.DEBUG) {
		defer func() {
			log.Debug("  <- %d plain bytes", n)
		}()
	}

	n, err = 0, nil
	for len(p) > 0 {
		var data []byte

		if cs.inPlainHeld != nil {
			data = cs.inPlainHeld
			cs.inPlainHeld = nil
		} else {
			if n == 0 {
				data = <-cs.inPlains
			} else {
				// Don't block if there's already anything to return.
				select {
				default:
					data = nil
				case data = <-cs.inPlains:
				}
			}
		}

		if data == nil {
			if n == 0 {
				err = io.EOF
			}
			return n, err
		}

		n += buf.CopyAdvance(&p, &data)
		if len(data) > 0 {
			cs.inPlainHeld = data
		}
	}

	return n, err
}

// Write queues the plaintext data p for transmission via cs, blocking if intermediary buffers are full.  The
// signature is that of io.Writer.Write.
func (cs *Session) Write(p []byte) (n int, err error) {
	if log.IsEnabledFor(logging.DEBUG) {
		defer func() {
			log.Debug("-> %d plain bytes", n)
		}()
	}

	// TODO: should we really do the framing here?  Right now, maxOutFrameDataSize is a kludge to make sure
	// very low-performance models don't get _too_ much delay from long frames.

	n, err = 0, nil
	for len(p) > 0 {
		data := p
		if len(data) > maxOutFrameDataSize {
			data = data[:maxOutFrameDataSize]
		}

		// After the send, the frame is owned by the other side and all the actual crypto will happen at
		// PullWrite time above.
		cs.outFrames <- newPlainDataFrame(data)
		p = p[len(data):]
		n += len(data)
	}

	if len(p) > 0 && err == nil {
		// Oops.
		err = io.ErrShortWrite
	}
	return n, err
}

func (cs *Session) Init(sinfo interface{}) error {
	cs.localPrivate = prim.NewPrivate()
	cs.serverInfo = sinfo
	cs.inPlains = make(chan []byte, 4)
	cs.outFrames = make(chan frame, 4)
	cs.inCipher.SetRandomKey()
	cs.outCipher.SetRandomKey()
	cs.outCryptPending = buf.CopyNew(cs.localPrivate.Public.Binary())
	cs.handshakeReassembly = buf.BeginReassembly(32)
	cs.state = stateHandshakeNoKey
	return nil
}

func beginAny(sinfo interface{}) (*Session, error) {
	cs := &Session{}
	if err := cs.Init(sinfo); err != nil {
		return nil, err
	}

	return cs, nil
}

// BeginClient starts a new crypting session from the client's perspective, given a server's public
// cryptographic parameters.
func BeginClient(pub *Public) (*Session, error) {
	return beginAny(pub)
}

// BeginServer starts a new crypting session from the server's perspective, given the server's private
// cryptographic parameters.
func BeginServer(priv *Private) (*Session, error) {
	return beginAny(priv)
}
