package Dust

import (
	"bytes"
	"crypto/cipher"
	"errors"
	"io"

	. "caolta3iaejox3z4madc5wmp4z.uuid/Dust4go/DustCrypto"
)

var (
	ErrBadHandshake = errors.New("bad handshake")
	ErrBadDecode = errors.New("bad decode")
)

const (
	maxInFrameSize = 65535
	maxOutFrameSize = 127
)

type cryptoFrame []byte

func newPlainDataFrame(data []byte) cryptoFrame {
	if len(data) > 65535 {
		panic("data too long for frame representation")
	}
	
	payloadSize := len(data) + 33
	frame := make([]byte, 2 + payloadSize)
	frame[0] = uint8(payloadSize >> 8)
	frame[1] = uint8(payloadSize & 0xff)
	frame[2] = 0x01
	copy(frame[3:], data)
	// Leave space for the authenticator, but don't actually compute it yet.
	return cryptoFrame(frame)
}

func newPaddingFrame(requestedSize int) cryptoFrame {
	size := requestedSize
	if size < 35 {
		size = 35
	}
	if size > 65535 {
		size = 65535
	}

	frame := make([]byte, size)
	payloadSize := len(frame) - 2
	frame[0] = uint8(payloadSize >> 8)
	frame[1] = uint8(payloadSize & 0xff)
	frame[2] = 0x00
	return cryptoFrame(frame)
}

func (frame cryptoFrame) authenticateWith(sessionKey SecretBytes) {
	// Eeeek, raw shared secret as HMAC key!
	authenticator := ComputeAuthenticator(frame[:len(frame)-32], sessionKey)
	copy(frame[len(frame)-32:], authenticator)
}

func (frame cryptoFrame) wellFormed() bool {
	if len(frame) < 35 {
		return false
	}
	return 2 + int(uint16(frame[0]) << 8 | uint16(frame[1])) == len(frame)
}

func (frame cryptoFrame) verifyAuthenticator(sessionKey SecretBytes) bool {
	// Eeeek, raw shared secret as HMAC key!
	return VerifyAuthenticator(frame[:len(frame)-32], sessionKey, frame[len(frame)-32:])
}

func (frame cryptoFrame) hasData() bool {
	return frame[2] & 1 != 0
}

func (frame cryptoFrame) data() []byte {
	return frame[3:len(frame)-32]
}

func (frame cryptoFrame) encryptWith(streamCipher cipher.Stream) {
	streamCipher.XORKeyStream(frame, frame)
}

func (frame cryptoFrame) slice() []byte {
	return frame
}

func (frame cryptoFrame) wireSize() int {
	return len(frame)
}

func maybeExpectedFrameSize(possibleFrame []byte) int {
	if 2 <= len(possibleFrame) {
		return 2 + int(uint16(possibleFrame[0]) << 8 | uint16(possibleFrame[1]))
	} else {
		return -1
	}
}

type cryptoState int

const (
	cryptoStateFailed cryptoState = iota
	cryptoStateStreaming 
	cryptoStateHandshakeClient
	cryptoStateHandshakeServer
)

type CryptoSession struct {
	// This is one of:
	// 
	//   - Failed :: we're using a bogus session key to send, and received data is discarded.
	// 
	//   - Streaming :: we have a valid session key.  Sending and receiving are both enabled.
	// 
	//       - No IV (when handshakeReassembly set, inCipher unset) :: reassembling an IV in the
	//         handshakeReassembly buffer; no data yet.
	// 
	//       - With IV (when handshakeReassembly unset, inCipher set) :: we are now stream-decrypting
	//         incoming data and extracting frames from it.
	//
	//   - HandshakeClient :: the first part of the handshake has been pre-queued for output,
	//     and the server's response is being reassembled in handshakeReassembly.  No session
	//     key, so we can't send anything yet.
	//
	//   - HandshakeServer :: the client's handshake request is being reassembled in handshakeReassembly.
	//     No session key, so we can't send anything yet.
	state cryptoState
	
	// Always set.
	//   - If state is HandshakeClient, this is a *CryptoServerIdentity.
	//   - If state is HandshakeServer, this is a *CryptoServerPrivate.
	serverInfo interface{}

	// These both correspond to ephemeral keys.  The server's long-term key or keypair will be
	// stashed somewhere in serverInfo.  localPair is always set.  remotePublic is set if state is
	// Streaming.
	localPair KeyPair
	remotePublic PublicKey
	
	// Set if state is Streaming or Failed.
	sessionKey SecretBytes

	// Incoming data must be processed against this first, if set, before inCipher.  cap() is the
	// expected reassembly length, and the semantic is determined by state:
	//
	//   - Streaming :: reassembling an IV
	//   - HandshakeClient :: reassembling a server response
	//   - HandshakeServer :: reassembling a client request
	handshakeReassembly []byte
	
	// Set if state is Streaming.
	inCipher cipher.Stream

	// Decrypted data is reassembled into frames here.  cap() is the maximum reassembly length.
	dataReassembly []byte

	// Stream-oriented byte chunks are sent to inPlains from the outward-facing side of the crypto
	// session.  The receiver owns the chunks.  inPlainHeld is controlled by the receiver methods from
	// the inward-facing side, and is implicitly prepended to receiving from inPlains.
	inPlainHeld []byte
	inPlains chan []byte

	// Implicitly prepended to any other outgoing data; these bytes are not encrypted before sending.
	outCryptPending []byte
	
	// Set if state is Streaming or Failed.
	outCipher cipher.Stream

	// Plaintext frames are sent to outFrames from the inward-facing side of the crypto session.  The
	// receiver owns the chunks.  Space is allocated for an authenticator, but it is not computed yet, and
	// nor is the frame encrypted.
	outFrames chan cryptoFrame
}

// Copy as many bytes as possible from *in to *out, and advance the start pointers of both *in and *out past
// the copied bytes.
func copyAdvance(out *[]byte, in *[]byte) int {
	n := copy(*out, *in)
	*in = (*in)[n:]
	*out = (*out)[n:]
	return n
}

func copyReassemble(out *[]byte, in *[]byte, maybeMax int) int {
	var avail []byte
	if 0 <= maybeMax {
		if maybeMax <= len(*out) {
			return 0
		}
		avail = (*out)[len(*out):maybeMax]
	} else {
		avail = (*out)[len(*out):cap(*out)]
	}

	n := copy(avail, *in)
	*in = (*in)[n:]
	*out = (*out)[:len(*out)+n]
	return n
}

func fixedSizeReassemblyComplete(reassembly []byte) bool {
	return len(reassembly) == cap(reassembly)
}

func beginReassembly(size int) []byte {
	return make([]byte, 0, size)
}

func copyNew(slice []byte) []byte {
	out := make([]byte, len(slice))
	copy(out, slice)
	return out
}

// PullWrite tries to pull post-encryption bytes into p, using in-band framing to intersperse any plain data
// available to write with enough padding frames to completely fill p immediately.  In some handshake states,
// err may be set to ErrNoProgress with n < len(p) to indicate that we actually can't send anything more,
// which is kind of terrible.  Note that we may still have n > 0 in that case.  This method must be called
// from the outward-facing side of the CryptoSession.
func (cs *CryptoSession) PullWrite(p []byte) (n int, err error) {
	n, err = 0, nil
	for len(p) > 0 {
		if len(cs.outCryptPending) > 0 {
			n += copyAdvance(&p, &cs.outCryptPending)
			continue
		}

		var frame cryptoFrame
		
		switch cs.state {
		case cryptoStateStreaming:
			select {
			default:
				frame = newPaddingFrame(len(p))
			case frame = <-cs.outFrames:
			}
		case cryptoStateFailed:
			// TODO: do we have to drain cs.outFrames here?
			frame = newPaddingFrame(len(p))
		default:
			// In the middle of some kind of handshakey thing.  The stream ciphers aren't inited
			// yet, so we can't send anything.  Argh.
			err = io.ErrNoProgress
			return n, err
		}

		if len(frame) < 32 {
			panic("frame too small?!")
		}

		// We own frame forever now, and outCipher is valid.  Authenticate and encrypt in-place.
		frame.authenticateWith(cs.sessionKey)
		frame.encryptWith(cs.outCipher)
		remaining := frame.slice()
		n += copyAdvance(&p, &remaining)
		if len(remaining) > 0 {
			cs.outCryptPending = append(cs.outCryptPending, remaining...)
		}
	}

	return n, err
}

// Move from any state to the Failed state, setting a bogus session key and destroying any in-progress data.
func (cs *CryptoSession) fail() {
	cs.state = cryptoStateFailed
	cs.handshakeReassembly = nil
	cs.dataReassembly = nil
	
	bogusKey := NewSecretBytes()
	if err := RandomizeSecretBytes(bogusKey); err != nil {
		// TODO: steal some extra entropy earlier to prevent this case.  (We do actually have to
		// panic in the absence of that because otherwise we start emitting predictable-ish bytes.)
		panic("Nooooo what happened to my entropy source, oh god")
	}
	
	cs.sessionKey = bogusKey
	cs.outCipher = NewStreamCipher(bogusKey, [32]byte{})
	cs.outCryptPending = nil
}

// Called whenever an invalid handshake record is received.
func (cs *CryptoSession) failHandshake() {
	debugf("CS: fail handshake")
	cs.fail()
}

// Called whenever an undecodable frame is received.
func (cs *CryptoSession) failDecode() {
	debugf("CS: fail decode")
	cs.fail()
}

// Change to the Streaming state with the given sessionKey, setting up to encrypt outgoing data and receive a
// stream of IV + data.
func (cs *CryptoSession) beginStreaming(sessionKey SecretBytes) {
	cs.sessionKey = sessionKey
	cs.state = cryptoStateStreaming
	cs.handshakeReassembly = beginReassembly(32)
	
	// This part isn't actually that secret, just the randomize function has that type.
	outIV := NewSecretBytes()
	if err := RandomizeSecretBytes(outIV); err != nil {
		// TODO: get this entropy earlier
		cs.failHandshake()
		return
	}

	// Eeeek, raw shared secret as stream cipher key!
	cs.outCipher = NewStreamCipher(cs.sessionKey, *outIV.Pointer())
	cs.outCryptPending = append(cs.outCryptPending, outIV.Slice()...)
}

// The incoming IV has been assembled in the handshakeReassembly buffer.  The session key has already been
// set.  Set up to decrypt incoming data.
func (cs *CryptoSession) beginInCipher() {
	inIV := [32]byte{}
	copied := copy(inIV[:], cs.handshakeReassembly)
	if copied != 32 {
		panic("should not have gotten here without incoming IV")
	}

	// Eeeek, raw shared secret as stream cipher key!
	cs.inCipher = NewStreamCipher(cs.sessionKey, inIV)
	cs.dataReassembly = beginReassembly(maxInFrameSize + 2 + 32)
	cs.handshakeReassembly = nil
}

// TODO: recheck what else to destroy when completing handshakes

// A server handshake response has been assembled in the handshakeReassembly buffer; process it and attempt to
// move to the Streaming state.
func (cs *CryptoSession) completeHandshakeClient() {
	response := cs.handshakeReassembly
	cs.handshakeReassembly = nil
	
	if len(response) != 64 {
		panic("should not have gotten here without a complete response")
	}

	var err error
	cs.remotePublic, err = LoadPublicKeyUniform(response[0:32])
	if err != nil {
		panic("should always be able to load public key here")
	}
	sid := cs.serverInfo.(*CryptoServerIdentity)

	rawDH := NewSecretBytes()
	defer rawDH.Destroy()
	sk := NewDigest()
	defer sk.Destroy()
	cs.localPair.ECDH(cs.remotePublic, rawDH)
	sk.WriteSecret(rawDH)
	cs.localPair.ECDH(sid.longtermPublic, rawDH)
	sk.WriteSecret(rawDH)
	rawDH.Destroy()
	sk.Write(bytes.Join([][]byte{
		sid.idBytes,
		cs.localPair.Public().Bytes(),
		cs.remotePublic.Bytes(),
		[]byte(`ntor`),
	}, []byte{}))

	var sessionKey SecretBytes
	defer func() {
		if cs.state != cryptoStateStreaming && sessionKey != nil {
			sessionKey.Destroy()
		}
	}()
	sessionKey = NewSecretBytes()
	sk.SumSecret(sessionKey)

	confirmationInput := bytes.Join([][]byte{
		sid.idBytes,
		cs.remotePublic.Bytes(),
		cs.localPair.Public().Bytes(),
		[]byte(`ntorserver`),
	}, []byte{})

	// Eeeek, raw shared secret as HMAC key!
	confirmationOk := VerifyAuthenticator(confirmationInput, sessionKey, response[32:64])
	if !confirmationOk {
		cs.failHandshake()
		return
	}

	cs.beginStreaming(sessionKey)
}

// A client handshake request has been assembled in the handshakeReassembly buffer; process it and attempt to
// move to the Streaming state.
func (cs *CryptoSession) completeHandshakeServer() {
	request := cs.handshakeReassembly
	cs.handshakeReassembly = nil
	
	if len(request) != 32 {
		panic("should not have gotten here without a complete request")
	}

	var err error
	cs.remotePublic, err = LoadPublicKeyUniform(request[0:32])
	if err != nil {
		panic("should always be able to load public key here")
	}
	spriv := cs.serverInfo.(*CryptoServerPrivate)

	rawDH := NewSecretBytes()
	defer rawDH.Destroy()
	sk := NewDigest()
	defer sk.Destroy()
	cs.localPair.ECDH(cs.remotePublic, rawDH)
	sk.WriteSecret(rawDH)
	spriv.longtermPair.ECDH(cs.remotePublic, rawDH)
	sk.WriteSecret(rawDH)
	rawDH.Destroy()
	sk.Write(bytes.Join([][]byte{
		spriv.idBytes,
		cs.remotePublic.Bytes(),
		cs.localPair.Public().Bytes(),
		[]byte(`ntor`),
	}, []byte{}))

	var sessionKey SecretBytes
	defer func() {
		if cs.state != cryptoStateStreaming && sessionKey != nil {
			sessionKey.Destroy()
		}
	}()
	sessionKey = NewSecretBytes()
	sk.SumSecret(sessionKey)

	confirmationInput := bytes.Join([][]byte{
		spriv.idBytes,
		cs.localPair.Public().Bytes(),
		cs.remotePublic.Bytes(),
		[]byte(`ntorserver`),
	}, []byte{})

	// Eeeek, raw shared secret as HMAC key!
	confirmation := ComputeAuthenticator(confirmationInput, sessionKey)
	response := append(cs.localPair.Public().UniformBytes(), confirmation...)
	if len(response) != 64 {
		panic("something grotesquely wrong with handshake response computation")
	}
	cs.outCryptPending = append(cs.outCryptPending, response...)

	cs.beginStreaming(sessionKey)
}

// Attempt to decode exactly one plaintext frame.  If it has any data, pass it through to the inward-facing side.
func (cs *CryptoSession) decodePlainFrame(p []byte) error {
	frame := cryptoFrame(p)
	// TODO: discard malformed frames after or before completing them?
	if !frame.wellFormed() {
		return ErrBadDecode
	}

	// TIMING: Always check the authenticator, to minimize timing surface... at least unless the Go
	// compiler decides to sink the VerifyAuthenticator down one of the branches.  Aaargh.
	//
	// TODO: fail entire stream if verification of any frame fails?  Not right now, because the spec says
	// authenticators for padding frames are allowed to be blank.
	authenticated := frame.verifyAuthenticator(cs.sessionKey)
	dataCarrying := frame.hasData()
	if dataCarrying && authenticated {
		cs.inPlains <- copyNew(frame.data())
	}

	return nil
}

// PushRead processes new pre-decryption bytes in p, handling handshake completions and sending any usable
// plaintext that results to the inward-facing side of the CryptoSession.  This method must be called from the
// outward-facing side of the CryptoSession.  The signature is similar to io.Writer.Write.
func (cs *CryptoSession) PushRead(p []byte) (n int, err error) {
	n, err = 0, nil

	for len(p) > 0 {
		// TIMING: arguably this leads to a timing attack against whether a handshake succeeded if the
		// attacker can measure our CPU usage, but that's probably not preventable anyway?  Come back to this
		// later.
		if cs.state == cryptoStateFailed {
			return n+len(p), err
		}
		
		if cs.handshakeReassembly != nil {
			n += copyReassemble(&cs.handshakeReassembly, &p, -1)
			if fixedSizeReassemblyComplete(cs.handshakeReassembly) {
				switch cs.state {
				case cryptoStateHandshakeClient:
					cs.completeHandshakeClient()
				case cryptoStateHandshakeServer:
					cs.completeHandshakeServer()
				case cryptoStateStreaming:
					cs.beginInCipher()
				}
			}
		} else if cs.inCipher == nil {
			// If we were in a handshake or streaming no-IV state, we did a handshake reassembly
			// cycle above and shouldn't be here.  If we were in a failed state, we've already
			// returned.  So this isn't a valid state.
			panic("should not have gotten here without incoming cipher")
		} else {
			expectedFrameSize := maybeExpectedFrameSize(cs.dataReassembly)
			subn := copyReassemble(&cs.dataReassembly, &p, expectedFrameSize)
			n += subn
			newSlice := cs.dataReassembly[len(cs.dataReassembly)-subn:]
			cs.inCipher.XORKeyStream(newSlice, newSlice)

			possibleFrame := cs.dataReassembly
			expectedFrameSize = maybeExpectedFrameSize(possibleFrame)
			if !(0 <= expectedFrameSize && expectedFrameSize <= len(possibleFrame)) {
				continue
			}
			
			frame := possibleFrame[:expectedFrameSize]
			err = cs.decodePlainFrame(frame)
			// TODO: this might be slow.
			remaining := copy(cs.dataReassembly, cs.dataReassembly[len(frame):])
			cs.dataReassembly = cs.dataReassembly[:remaining]
			if err != nil {
				cs.failDecode()
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
func (cs *CryptoSession) Read(p []byte) (n int, err error) {
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

		n += copyAdvance(&p, &data)
		if len(data) > 0 {
			cs.inPlainHeld = data
		}
	}

	return n, err
}

// Write queues the plaintext data p for transmission via cs, blocking if intermediary buffers are full.  The
// signature is that of io.Writer.Write.
func (cs *CryptoSession) Write(p []byte) (n int, err error) {
	// TODO: should we really do the framing here?  Right now, maxOutFrameSize is a kludge to make sure
	// very low-performance models don't get _too_ much delay from long frames.
	
	n, err = 0, nil
	for len(p) > 0 {
		data := p
		if len(data) > maxOutFrameSize {
			data = data[:maxOutFrameSize]
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

func (cs *CryptoSession) Init(state cryptoState, serverInfo interface{}) error {
	var err error
	if cs.localPair, err = NewKeyPair(); err != nil {
		return err
	}

	cs.state = state
	cs.serverInfo = serverInfo
	cs.inPlains = make(chan []byte, 4)
	cs.outFrames = make(chan cryptoFrame, 4)
	return nil
}

func BeginCryptoClient(sid *CryptoServerIdentity) (*CryptoSession, error) {
	var err error
	cs := &CryptoSession{}
	if err = cs.Init(cryptoStateHandshakeClient, sid); err != nil {
		return nil, err
	}

	cs.outCryptPending = cs.localPair.Public().UniformBytes()
	cs.handshakeReassembly = beginReassembly(64)
	return cs, nil
}

func BeginCryptoServer(spriv *CryptoServerPrivate) (*CryptoSession, error) {
	var err error
	cs := &CryptoSession{}
	if err = cs.Init(cryptoStateHandshakeServer, spriv); err != nil {
		return nil, err
	}

	cs.handshakeReassembly = beginReassembly(32)
	return cs, nil
}
