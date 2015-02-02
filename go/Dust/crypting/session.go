package crypting

import (
	"crypto/cipher"
	"errors"
	"io"

	"github.com/blanu/Dust/go/Dust/bufman"
	"github.com/blanu/Dust/go/Dust/cryptions"
)

var (
	ErrIncompleteFrame = errors.New("Dust/crypting: incomplete frame")
	ErrBadHandshake    = errors.New("Dust/crypting: bad handshake")
	ErrBadDecode       = errors.New("Dust/crypting: bad decode")
	ErrStuck           = errors.New("Dust/crypting: cannot get anything to send here")
)

const (
	kdfC2S = `c2s.`
	kdfS2C = `s2c.`

	kdfCipherData = `hide`
	kdfMACData = `chck`
	kdfMACConfirm = `play`
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
	// stashed somewhere in serverInfo.  localPair is always set.  remotePublic is set if state is
	// HandshakeKey.
	localPair    cryptions.KeyPair
	remotePublic cryptions.PublicKey

	// Incoming data must be processed against this first, if set, before inCipher.  When HandshakeNoKey:
	// reassembling an ephemeral public key.  When HandshakeKey: searching for the confirmation code from
	// the remote side.
	handshakeReassembly bufman.Reassembly

	// Set in HandshakeKey state.
	inConfirmation cryptions.SecretBytes

	// Set in Streaming state.
	inCipher cipher.Stream
	inMAC    cryptions.MAC

	// Decrypted data is reassembled into frames here.  The reassembly capacity is the maximum frame
	// wire size.
	dataReassembly bufman.Reassembly

	// Stream-oriented byte chunks are sent to inPlains from the outward-facing side of the crypto
	// session.  The receiver owns the chunks.  inPlainHeld is controlled by the receiver methods from
	// the inward-facing side, and is implicitly prepended to any receive from inPlains.
	inPlainHeld []byte
	inPlains    chan []byte

	// Implicitly prepended to any other outgoing data; these bytes are not encrypted before sending.
	outCryptPending []byte

	// Always set.  For any state except Streaming, uses a random key.
	outCipher cipher.Stream

	// Set in Streaming state.
	outMAC cryptions.MAC

	// Plaintext frames are sent to outFrames from the inward-facing side of the crypto session.  The
	// receiver owns the chunks.  Space is allocated for an authenticator, but it is not computed yet, and
	// nor is the frame encrypted.
	outFrames chan frame
}

// PullWrite tries to pull post-encryption bytes into p, using in-band framing to intersperse any plain data
// available to write with enough padding frames to completely fill p immediately.  In some handshake states,
// err may be set to ErrStuck with n < len(p) to indicate that we actually can't send anything more,
// which is kind of terrible.  Note that we may still have n > 0 in that case.  This method must be called
// from the outward-facing side of the Session.  The signature is similar to that of io.Reader.Read.
func (cs *Session) PullWrite(p []byte) (n int, err error) {
	n, err = 0, nil
	for len(p) > 0 {
		if len(cs.outCryptPending) > 0 {
			n += bufman.CopyAdvance(&p, &cs.outCryptPending)
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

			if len(p)%32 != 0 {
				leftover := make([]byte, 32 - len(p)%32)
				cs.outCipher.XORKeyStream(leftover, leftover)
				cs.outCryptPending = append(cs.outCryptPending, leftover...)
			}

			n += len(p)
			return n, err
		}

		if len(frame) < 32 {
			panic("Dust/crypting: frame too small?!")
		}

		// We own frame forever now, and outCipher is valid.  Authenticate and encrypt in-place.
		cs.outMAC.Reset()
		frame.authenticateWith(cs.outMAC)
		frame.encryptWith(cs.outCipher)
		remaining := frame.slice()
		n += bufman.CopyAdvance(&p, &remaining)
		if len(remaining) > 0 {
			cs.outCryptPending = append(cs.outCryptPending, remaining...)
		}
	}

	return n, err
}

// Move from any state to the Failed state, setting a bogus session key and destroying any in-progress data.
func (cs *Session) fail() {
	cs.state = stateFailed
	cs.handshakeReassembly = nil
	cs.dataReassembly = nil

	bogusKey := cryptions.NewSecretBytes()
	if err := cryptions.RandomizeSecretBytes(bogusKey); err != nil {
		// TODO: steal some extra entropy earlier to prevent this case.  (We do actually have to
		// panic in the absence of that because otherwise we start emitting predictable-ish bytes.)
		panic("Dust/crypting: Nooooo what happened to my entropy source, oh god")
	}

	cs.outCipher = cryptions.NewStreamCipher(bogusKey, [32]byte{})
	cs.outCryptPending = nil
	cs.outMAC = nil
}

func (cs *Session) receivedEphemeralKey() {
	var err error

	received := cs.handshakeReassembly.Data()
	if len(received) != 32 {
		panic("Dust/crypting: should not have gotten here without 32-byte handshake")
	}

	cs.remotePublic, err = cryptions.LoadPublicKeyUniform(received)
	if err != nil {
		panic("Dust/crypting: should always be able to load public key here")
	}

	rawDH := cryptions.NewSecretBytes()
	sk := cryptions.NewDigest()
	cs.localPair.ECDH(cs.remotePublic, rawDH)
	sk.WriteSecret(rawDH)

	var inKdfPrefix, outKdfPrefix string
	switch sinfo := cs.serverInfo.(type) {
	default:
		panic("Dust/crypting: bad serverInfo type")

	case *Public:
		inKdfPrefix = kdfS2C
		outKdfPrefix = kdfC2S
		cs.localPair.ECDH(sinfo.LongtermPublic, rawDH)
		sk.WriteSecret(rawDH)
		sk.Write(sinfo.IdBytes)
		sk.Write(cs.localPair.Public().Bytes())
		sk.Write(cs.remotePublic.Bytes())

	case *Private:
		inKdfPrefix = kdfC2S
		outKdfPrefix = kdfS2C
		sinfo.LongtermPair.ECDH(cs.remotePublic, rawDH)
		sk.WriteSecret(rawDH)
		sk.Write(sinfo.IdBytes)
		sk.Write(cs.remotePublic.Bytes())
		sk.Write(cs.localPair.Public().Bytes())
	}

	sk.Write([]byte(`ntor`))

	sessionSecret := cryptions.NewSecretBytes()
	sk.SumSecret(sessionSecret)

	// TODO: security proof or revision for confirmation code generation with new KDF structure.
	rawKey := cryptions.NewSecretBytes()
	cryptions.DeriveKey(sessionSecret, outKdfPrefix + kdfMACConfirm, rawKey)
	cs.outCryptPending = cryptions.NewMAC(rawKey).Sum(cs.outCryptPending)
	cryptions.DeriveKey(sessionSecret, outKdfPrefix + kdfCipherData, rawKey)
	cs.outCipher = cryptions.NewStreamCipher(rawKey, [32]byte{})
	cryptions.DeriveKey(sessionSecret, outKdfPrefix + kdfMACData, rawKey)
	cs.outMAC = cryptions.NewMAC(rawKey)

	// TODO: maybe only set these after checkConfirmation, for better defensive coding.
	cs.inConfirmation = cryptions.NewSecretBytes()
	cryptions.DeriveKey(sessionSecret, inKdfPrefix + kdfMACConfirm, rawKey)
	cryptions.NewMAC(rawKey).SumSecret(cs.inConfirmation)
	cryptions.DeriveKey(sessionSecret, inKdfPrefix + kdfCipherData, rawKey)
	cs.inCipher = cryptions.NewStreamCipher(rawKey, [32]byte{})
	cryptions.DeriveKey(sessionSecret, inKdfPrefix + kdfMACData, rawKey)
	cs.inMAC = cryptions.NewMAC(rawKey)

	cs.handshakeReassembly = bufman.BeginReassembly(32)
	cs.state = stateHandshakeKey
}

// Change to the Streaming state if we've reassembled a correct confirmation code.  Otherwise, throw away
// the 32-byte chunk and continue.
func (cs *Session) checkConfirmation() {
	if !cs.inConfirmation.Equal(cs.handshakeReassembly.Data()) {
		cs.handshakeReassembly.Reset()
		return
	}

	// We already set up all the derived keys when we received the ephemeral key.
	cs.handshakeReassembly = nil
	cs.inConfirmation = nil
	cs.dataReassembly = bufman.BeginReassembly(maxInFrameDataSize + 35)
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
	cs.inMAC.Reset()
	authenticated := frame.verifyAuthenticator(cs.inMAC)
	dataCarrying := frame.hasData()
	if dataCarrying && authenticated {
		// If there isn't enough buffer space in the channel, just drop the frame.  We cannot afford
		// to apply backpressure, as that would break the model.
		select {
		case cs.inPlains <- bufman.CopyNew(frame.data()):
		default:
		}
	}

	return frame.wireSize(), nil
}

// PushRead processes new pre-decryption bytes in p, handling handshake completions and sending any usable
// plaintext that results to the inward-facing side of the Session.  This method must be called from the
// outward-facing side of the Session.  The signature is similar to io.Writer.Write.
func (cs *Session) PushRead(p []byte) (n int, err error) {
	n, err = 0, nil

	for len(p) > 0 {
		// TIMING: arguably this leads to a timing attack against whether a handshake succeeded if the
		// attacker can measure our CPU usage, but that's probably not preventable anyway?  Come back to this
		// later.
		if cs.state == stateFailed {
			return n + len(p), err
		}

		if cs.handshakeReassembly != nil {
			n += bufman.CopyReassemble(&cs.handshakeReassembly, &p)
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
		} else if cs.inCipher == nil {
			// If we were in a handshake or streaming no-IV state, we did a handshake reassembly
			// cycle above and shouldn't be here.  If we were in a failed state, we've already
			// returned.  So this isn't a valid state.
			panic("Dust/crypting: should not have gotten here without incoming cipher")
		} else {
			n += bufman.TransformReassemble(&cs.dataReassembly, &p, cs.inCipher.XORKeyStream)

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

		n += bufman.CopyAdvance(&p, &data)
		if len(data) > 0 {
			cs.inPlainHeld = data
		}
	}

	return n, err
}

// Write queues the plaintext data p for transmission via cs, blocking if intermediary buffers are full.  The
// signature is that of io.Writer.Write.
func (cs *Session) Write(p []byte) (n int, err error) {
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
	var err error
	if cs.localPair, err = cryptions.NewKeyPair(); err != nil {
		return err
	}

	bogusKey := cryptions.NewSecretBytes()
	if err := cryptions.RandomizeSecretBytes(bogusKey); err != nil {
		return err
	}

	cs.state = stateHandshakeNoKey
	cs.serverInfo = sinfo
	cs.inPlains = make(chan []byte, 4)
	cs.outFrames = make(chan frame, 4)
	cs.outCipher = cryptions.NewStreamCipher(bogusKey, [32]byte{})
	cs.outCryptPending = bufman.CopyNew(cs.localPair.Public().UniformBytes())
	cs.handshakeReassembly = bufman.BeginReassembly(32)
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
