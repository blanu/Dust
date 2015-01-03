package Dust

import (
	"bytes"
	"crypto/cipher"
	"errors"
	"io"
	"net"
	"os"
	
	. "caolta3iaejox3z4madc5wmp4z.uuid/dust4go/DustCrypto"
)

type CryptoServerIdentity struct {
	idBytes []byte
	longtermPublic PublicKey
}

type CryptoServerPrivate struct {
	idBytes []byte
	longtermPair KeyPair
}

func (spriv *CryptoServerPrivate) DestroyPrivate() {
	spriv.longtermPair.DestroyPrivate()
}

var (
	ErrUnrecognizedAddressType = errors.New("unrecognized address type")
	ErrBadHandshake = errors.New("bad handshake")
	ErrBadDecode = errors.New("bad decode")
	ErrMissingParameters = errors.New("missing parameters")
)

func cryptoIdBytes(netAddr interface{}) ([]byte, error) {
	var ip net.IP
	var port int
	var l3Flag uint8
	var l4Flag uint8
	
	switch a := netAddr.(type) {
	default:
		return nil, ErrUnrecognizedAddressType
	case *net.TCPAddr:
		ip, port = a.IP, a.Port
		l4Flag = 0x00
	}

	// INCOMPLETE: this is wrong, as sometimes net.IP stores 16-octet slices with IPv4-mapped format.  Ugh.
	switch len(ip) {
	default:
		return nil, ErrUnrecognizedAddressType
	case net.IPv4len:
		l3Flag = 0x00
	case net.IPv6len:
		l3Flag = 0x01
	}

	idBytes := bytes.Join([][]byte{
		[]byte{l3Flag | l4Flag}, ip,
		[]byte{uint8(port >> 8), uint8(port & 0xff)},
	}, []byte{})
	return idBytes, nil
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
	// expected reassembly length, and the semantic is determined by state.
	handshakeReassembly []byte
	
	// Set if state is Streaming.
	inCipher cipher.Stream

	// Decrypted data is reassembled into frames here.
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

	// Preframed but plaintext chunks are sent to outFrames from the inward-facing side of the crypto
	// session.  The receiver owns the chunks.  Space is allocated for an authenticator, but it is not
	// computed yet, and nor is the frame encrypted.
	outFrames chan []byte
}

// TODO: make frames their own type
func (cs *CryptoSession) plainPaddingFrame(n int) []byte {
	if n < 35 {
		n = 35
	}
	if n > 65535 {
		n = 65535
	}
	
	frame := make([]byte, n)
	payloadSize := len(frame) - 2
	frame[0] = uint8(payloadSize >> 8)
	frame[1] = uint8(payloadSize & 0xff)
	frame[2] = 0x00
	return frame
}

// PullWrite tries to pull post-encryption bytes into p, using in-band framing to intersperse any plain data
// available to write with enough padding frames to completely fill p immediately.  In some handshake states,
// err may be set to ErrNoProgress to indicate that we actually can't send anything, which is kind of
// terrible.  This method must be called from the outer side of the CryptoSession.
func (cs *CryptoSession) PullWrite(p []byte) (n int, err error) {
	n, err = 0, nil
	for len(p) > 0 {
		if len(cs.outCryptPending) > 0 {
			copied := copy(p, cs.outCryptPending)
			cs.outCryptPending = cs.outCryptPending[copied:]
			if len(cs.outCryptPending) == 0 {
				cs.outCryptPending = nil
			}
			p = p[copied:]
			n += copied
			continue
		}

		var frame []byte
		
		switch cs.state {
		case cryptoStateStreaming:
			select {
			default:
				frame = cs.plainPaddingFrame(len(p))
			case frame = <-cs.outFrames:
			}
		case cryptoStateFailed:
			// TODO: do we have to drain cs.outFrames here?
			frame = cs.plainPaddingFrame(len(p))
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
		authenticator := ComputeAuthenticator(frame[:len(frame)-32], cs.sessionKey)
		copy(frame[len(frame)-32:], authenticator)
		cs.outCipher.XORKeyStream(frame, frame)
		copied := copy(p, frame)
		p = p[copied:]
		if copied < len(frame) {
			if len(cs.outCryptPending) > 0 {
				panic("unexpected outCryptPending stacking")
			}
			cs.outCryptPending = frame[copied:]
		}
		n += copied
	}

	return n, err
}

func (cs *CryptoSession) failDecode() {
	cs.state = cryptoStateFailed
	cs.dataReassembly = nil
}

func (cs *CryptoSession) failHandshake() {
	cs.state = cryptoStateFailed
	cs.handshakeReassembly = nil
	infof("failing handshake")
	
	bogusKey := NewSecretBytes()
	if err := RandomizeSecretBytes(bogusKey); err != nil {
		// TODO: steal some extra entropy earlier to prevent this case.  (We do actually have to
		// panic in the absence of that because otherwise we start emitting predictable-ish bytes.)
		panic("Nooooo what happened to my entropy source, oh god")
	}
	
	// We do need to set sessionKey for authenticator computation.
	cs.sessionKey = bogusKey
	cs.outCipher = NewStreamCipher(bogusKey, [32]byte{})
	cs.outCryptPending = make([]byte, 0)
}

func (cs *CryptoSession) beginInStream() {
	inIV := [32]byte{}
	copied := copy(inIV[:], cs.handshakeReassembly)
	if copied != 32 {
		panic("should not have gotten here without incoming IV")
	}

	// Eeeek, raw shared secret as stream cipher key!
	cs.inCipher = NewStreamCipher(cs.sessionKey, inIV)
	cs.dataReassembly = make([]byte, 0, 65535 + 2 + 32)
	cs.handshakeReassembly = nil
}

func (cs *CryptoSession) beginOutStream() {
	// This part isn't as secret, just the randomize function has that type.
	outIV := NewSecretBytes()
	if err := RandomizeSecretBytes(outIV); err != nil {
		cs.failHandshake()
		return
	}

	// Eeeek, raw shared secret as stream cipher key!
	cs.outCipher = NewStreamCipher(cs.sessionKey, *outIV.Pointer())
	cs.outCryptPending = append(cs.outCryptPending, outIV.Slice()...)
}

// TODO: recheck what else to destroy when completing handshakes

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

	cs.state = cryptoStateStreaming
	cs.sessionKey = sessionKey
	cs.handshakeReassembly = make([]byte, 0, 32)
	cs.beginOutStream()
}

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

	cs.state = cryptoStateStreaming
	cs.outCryptPending = response
	cs.sessionKey = sessionKey
	cs.handshakeReassembly = make([]byte, 0, 32)
	cs.beginOutStream()
}

// PushRead processes new pre-decryption bytes in p, handling handshake completions and sending any usable
// plaintext that results to the inner side of the CryptoSession.  This method must be called from the outer
// side of the CryptoSession.
func (cs *CryptoSession) PushRead(p []byte) (n int, err error) {
	// TIMING: arguably this leads to a timing attack against whether a handshake succeeded if the
	// attacker can measure our CPU usage, but that's probably not preventable anyway?  Come back to this
	// later.
	if cs.state == cryptoStateFailed {
		return len(p), nil
	}

	n, err = 0, nil

	for cs.handshakeReassembly != nil && len(p) > 0 {
		copied := copy(cs.handshakeReassembly[len(cs.handshakeReassembly):cap(cs.handshakeReassembly)], p)
		n += copied
		p = p[copied:]
		cs.handshakeReassembly = cs.handshakeReassembly[:len(cs.handshakeReassembly)+copied]
		if len(cs.handshakeReassembly) == cap(cs.handshakeReassembly) {
			switch cs.state {
			case cryptoStateHandshakeClient:
				cs.completeHandshakeClient()
			case cryptoStateHandshakeServer:
				cs.completeHandshakeServer()
			case cryptoStateStreaming:
				cs.beginInStream()
			}

			if cs.state == cryptoStateFailed {
				return n+len(p), err
			}

			if cs.handshakeReassembly != nil && len(cs.handshakeReassembly) == cap(cs.handshakeReassembly) {
				// TODO: more graceful
				panic("no progress on handshake")
			}
		}
	}

	if len(p) == 0 {
		return n, err
	}

	if cs.inCipher == nil {
		panic("should not have gotten here without incoming cipher")
	}

	for len(p) > 0 {
		avail := cs.dataReassembly[len(cs.dataReassembly):cap(cs.dataReassembly)]
		canCopy := len(p)
		if len(avail) < canCopy {
			canCopy = len(p)
		}
		cs.inCipher.XORKeyStream(avail[:canCopy], p[:canCopy])
		p = p[canCopy:]
		plain := cs.dataReassembly[0:len(cs.dataReassembly)+canCopy]
		cs.dataReassembly = plain
		n += canCopy

		if len(plain) < 2 {
			break
		}
		payloadSize := int(uint16(plain[0]) << 8 | uint16(plain[1]))
		
		if len(plain) < 2 + payloadSize {
			break
		}
		frame := plain[:2+payloadSize]

		// TODO: recheck this---discard malformed frames after or before completing them?
		if payloadSize < 33 {
			cs.failDecode()
			err = ErrBadDecode
			return n, err
		}

		infof("attempting to decode a frame of %d", 2+payloadSize)
		
		// TIMING: Always check the authenticator, to minimize timing surface... at least
		// unless the Go compiler decides to sink the VerifyAuthenticator down one of the
		// branches.  Aaargh.
		//
		// TODO: fail entire stream if verification of any frame fails?  Not right now,
		// because the spec says authenticators for padding frames are allowed to be blank.
		// 
		// Eeeek, raw shared secret as authenticator key!
		verificationOk := VerifyAuthenticator(
			frame[:len(frame)-32], cs.sessionKey, frame[len(frame)-32:])
		dataValid := (frame[2] & 1) != 0
		data := frame[3:len(frame)-32]
		infof("data %v, verify %v", dataValid, verificationOk)
		if dataValid && verificationOk {
			dataCopy := make([]byte, len(data))
			copy(dataCopy, data)
			cs.inPlains <- dataCopy
		}

		// TODO: this is probably slow.
		remaining := copy(cs.dataReassembly, cs.dataReassembly[len(frame):])
		cs.dataReassembly = cs.dataReassembly[:remaining]
	}

	return n, err
}

func (cs *CryptoSession) Read(p []byte) (n int, err error) {
	n, err = 0, nil
	for len(p) > 0 {
		var data []byte
		if cs.inPlainHeld != nil {
			data = cs.inPlainHeld
			cs.inPlainHeld = nil
		} else {
			infof("reading from inPlains")
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
			infof("got something from inPlains")
		}
		
		if data == nil {
			// TODO: handle abort from the other side.
			if n == 0 {
				err = io.EOF
			}
			break
		}

		copied := copy(p, data)
		p = p[copied:]
		n += copied
		if copied < len(data) {
			cs.inPlainHeld = data[copied:]
			break
		}
	}

	return n, err
}

func (cs *CryptoSession) Write(p []byte) (n int, err error) {
	n, err = 0, nil
	for len(p) > 0 {
		frameData := p
		if len(frameData) > 65535 {
			frameData = frameData[:65535]
		}

		payloadSize := len(frameData) + 33
		frame := make([]byte, 2 + payloadSize)
		frame[0] = uint8(payloadSize >> 8)
		frame[1] = uint8(payloadSize & 0xff)
		frame[2] = 0x01
		copy(frame[3:], frameData)
		// We leave space for the authenticator, but don't actually compute it because the session key
		// is owned by the other side.  It'll be computed in PullWrite.
		//
		// TODO: handle abort from the other side.
		cs.outFrames <- frame
		n += len(frameData)
		p = p[len(frameData):]
	}

	// TODO: set error on short write
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
	cs.outFrames = make(chan []byte, 1)
	return nil
}

func BeginCryptoClient(sid *CryptoServerIdentity) (*CryptoSession, error) {
	var err error
	cs := &CryptoSession{}
	if err = cs.Init(cryptoStateHandshakeClient, sid); err != nil {
		return nil, err
	}

	cs.outCryptPending = cs.localPair.Public().UniformBytes()
	cs.handshakeReassembly = make([]byte, 0, 64)
	return cs, nil
}

func BeginCryptoServer(spriv *CryptoServerPrivate) (*CryptoSession, error) {
	var err error
	cs := &CryptoSession{}
	if err = cs.Init(cryptoStateHandshakeServer, spriv); err != nil {
		return nil, err
	}

	cs.handshakeReassembly = make([]byte, 0, 32)
	return cs, nil
}

const publicKeyParam string = "p"

func LoadCryptoServerIdentityBridgeLine(
	address net.IP, port int,
	params map[string]string,
) (*CryptoServerIdentity, error) {
	publicString, found := params[publicKeyParam]
	if !found {
		return nil, ErrMissingParameters
	}

	idBytes, err := cryptoIdBytes(&net.TCPAddr{IP: address, Port: port})
	if err != nil {
		return nil, err
	}
	longtermPublic, err := LoadPublicKeyBase32(publicString)
	if err != nil {
		return nil, err
	}
	return &CryptoServerIdentity{idBytes, longtermPublic}, nil
}

func LoadCryptoServerPrivateFile(
	address net.IP, port int,
	path string,
) (*CryptoServerPrivate, error) {
	var file *os.File
	var err error
	defer func() {
		if file != nil {
			_ = file.Close()
		}
	}()
	
	if file, err = os.Open(path); err != nil {
		return nil, err
	}

	var privateRepr SecretBytes
	ok := false
	defer func() {
		if !ok && privateRepr != nil {
			privateRepr.Destroy()
		}
	}()
	privateRepr = NewSecretBytes()
	
	n, err := file.Read(privateRepr.Slice())
	if err != nil {
		return nil, err
	}
	if n != 32 {
		return nil, ErrBadPrivateKey
	}
	_ = file.Close()
	file = nil
	
	idBytes, err := cryptoIdBytes(&net.TCPAddr{IP: address, Port: port})
	if err != nil {
		return nil, err
	}
	
	longtermPair, err := LoadKeyPairOwningPrivate(privateRepr)
	privateRepr = nil
	if err != nil {
		return nil, err
	}

	spriv := &CryptoServerPrivate{idBytes, longtermPair}
	ok = true
	return spriv, nil
}

func NewCryptoServerPrivate(address net.IP, port int) (*CryptoServerPrivate, error) {
	idBytes, err := cryptoIdBytes(&net.TCPAddr{IP: address, Port: port})
	if err != nil {
		return nil, err
	}
	
	ok := false
	var longtermPair KeyPair
	defer func() {
		if !ok && longtermPair != nil {
			longtermPair.DestroyPrivate()
		}
	}()
	if longtermPair, err = NewKeyPair(); err != nil {
		return nil, err
	}

	spriv := &CryptoServerPrivate{idBytes, longtermPair}
	ok = true
	return spriv, nil
}

func (spriv *CryptoServerPrivate) SavePrivateFile(path string) error {
	var file *os.File
	var err error
	defer func() {
		if file != nil {
			file.Close()
		}
	}()
	
	if file, err = os.OpenFile(path, os.O_CREATE | os.O_EXCL | os.O_WRONLY, 0600); err != nil {
		return err
	}

	if _, err = file.Write(spriv.longtermPair.PrivateSlice()); err != nil {
		_ = file.Close()
		file = nil
		_ = os.Remove(path)
		return err
	}

	err = file.Close()
	file = nil
	if err != nil {
		return err
	}

	return nil
}

func (spriv *CryptoServerPrivate) BridgeParams() map[string]string {
	return map[string]string{
		publicKeyParam: spriv.longtermPair.Public().Base32(),
	}
}
