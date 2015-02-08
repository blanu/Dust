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

func (cs *Session) drainOutput() {
	for {
		select {
		case _ = <-cs.outPlains:
		default:
			return
		}
	}
}

func (cs *Session) pullZero(p []byte) {
	for i := 0; i < len(p); i++ {
		p[i] = 0
	}
	cs.outCipher.XORKeyStream(p, p)
	cs.outPosition += uint64(len(p))
}

func (cs *Session) pullData(p []byte) {
	// Should have nothing in outCrypted to start with.
	for len(p) > 0 {
		if cs.outCrypted.ValidLen() > 0 {
			panic("Dust/crypting: pullData with leftover crumbs in construction buffer")
		}

		var dgram []byte
		select {
		default:
			cs.pullZero(p)
			cs.outMAC.Write(p)
			return
		case dgram = <-cs.outPlains:
		}

		if len(dgram) > cs.MTU {
			// This should have been handled by Write(), so something's meddling with our
			// channels.
			panic("Dust/crypting: datagram with len > MTU escaped across the boundary")
		}

		tLen := 256 + len(dgram)
		header := []byte{uint8(tLen >> 8), uint8(tLen & 0xff)}
		headerN := cs.outCrypted.TransformIn(header, cs.outCipher.XORKeyStream)
		dgramN := cs.outCrypted.TransformIn(dgram, cs.outCipher.XORKeyStream)
		cs.outMAC.Write(cs.outCrypted.Data())
		mac := cs.outMAC.Generate()
		macN := cs.outCrypted.TransformIn(mac[:], cs.outCipher.XORKeyStream)
		if !(headerN == len(header) && dgramN == len(dgram) && macN == len(mac)) {
			panic("Dust/crypting: somehow not enough space in output buffer")
		}

		cs.outPosition += uint64(headerN + dgramN + macN)
		cs.outMAC.Reset(cs.outPosition)
		cs.outCrypted.CopyOut(&p)
	}
}

// PullWrite tries to pull post-encryption bytes into p, using in-band framing to intersperse any plain data
// available to write with enough padding frames to completely fill p immediately.  In some handshake states,
// err may be set to ErrStuck with n < len(p) to indicate that we actually can't send anything more,
// which is kind of terrible.  Note that we may still have n > 0 in that case.  This method must be called
// from the outward-facing side of the Session.  The signature is similar to that of io.Reader.Read.
func (cs *Session) PullWrite(p []byte) (n int, err error) {
	log.Debug("-> pulling %d bytes", len(p))

	n, err = len(p), nil
	if cs.outCrypted.ValidLen() > 0 {
		cs.outCrypted.CopyOut(&p)
	}

	if len(p) == 0 {
		// This can be true from before the possible CopyOut above, too, so don't move it inside
		// the above conditional.
		return
	}

	switch cs.state {
	default:
		panic("Dust/crypting: unhandled state!")

	case stateHandshakeNoKey:
		// Handshake interstitial data must be aligned 32-byte chunks.
		cs.pullZero(p)

		if len(p)%32 != 0 {
			cs.pullZero(cs.outCrypted.PreData(32 - len(p)%32))
		}

		log.Debug("-> handshake interstitial %d bytes", len(p))

	case stateFailed:
		cs.drainOutput()
		cs.pullZero(p)

	case stateStreaming, stateHandshakeKey:
		// TODO: this starts potentially sending data even before the remote confirmation code
		// has been received.  That might be important for reducing the number of round trips,
		// but...
		cs.pullData(p)
	}

	return
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

func (cs *Session) receivedEphemeralKey() {
	var err error

	received := cs.inHandshake.Data()
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
	outCSlice := outConfirmation[:]
	buf.CopyReassemble(&cs.outCrypted, &outCSlice)
	log.Debug("-> send confirmation starting with %v", outConfirmation[:2])
	cs.outCipher.SetKey(secret.DeriveCipherKey(outKdfPrefix + kdfCipherData))
	cs.outMAC.SetKey(secret.DeriveAuthKey(outKdfPrefix + kdfMACData))
	cs.outMAC.Reset(0)
	cs.inConfirmation = prim.GenerateMAC(confInput, 0, secret.DeriveAuthKey(inKdfPrefix+kdfMACConfirm))
	log.Debug("  <- expect confirmation starting with %v", cs.inConfirmation[:2])
	cs.inCipher.SetKey(secret.DeriveCipherKey(inKdfPrefix + kdfCipherData))
	cs.inMAC.SetKey(secret.DeriveAuthKey(inKdfPrefix + kdfMACData))
	cs.inMAC.Reset(0)

	cs.inHandshake = buf.BeginReassembly(32)
	cs.state = stateHandshakeKey
}

// Change to the Streaming state if we've reassembled a correct confirmation code.  Otherwise, throw away
// the 32-byte chunk and continue.
func (cs *Session) checkConfirmation() {
	if !cs.inConfirmation.Equal(cs.inHandshake.Data()) {
		//log.Debug("<- discarding not-a-confirmation")
		cs.inHandshake.Reset()
		return
	}

	// We already set up all the derived keys when we received the ephemeral key.
	log.Debug("  <- entering streaming state")
	cs.inHandshake = nil
	cs.inStreaming = buf.BeginReassembly(cs.MTU + frameOverhead)
	cs.inSequence = 1
	cs.inPosition = 0
	cs.outPosition = 0
	cs.state = stateStreaming
	return
}

func (cs *Session) continueUnframing(plain []byte, beforeCrypt int, crypt []byte) (n int) {
	// Postcondition: as much of crypt as is relevant has already been fed to inMAC, and
	// inPosition has been advanced past all of crypt.

	advanceMAC := func(subn int) {
		beforeCrypt -= subn
		if beforeCrypt < 0 {
			cs.inPosition += uint64(-beforeCrypt)
			cs.inMAC.Write(crypt[:-beforeCrypt])
			crypt = crypt[-beforeCrypt:]
			beforeCrypt = 0
		}
	}

	consume := func(subn int) {
		plain = plain[subn:]
		n += subn
	}

	// Loop invariant: crypt[beforeCrypt:] has the same stream position as plain.
	for len(plain) > 0 {
		log.Debug("have %d plains, %d+%d crypts", len(plain), beforeCrypt, len(crypt))
		if plain[0] == 0 {
			k := 1
			for k < len(plain) && plain[k] == 0 {
				k++
			}
			advanceMAC(k)
			consume(k)
		} else if len(plain) < 2 {
			advanceMAC(len(plain))
			break
		} else {
			tLen := int(uint16(plain[0])<<8 | uint16(plain[1]))
			dgramLen := tLen - 256
			if dgramLen > cs.MTU {
				log.Error("datagram received longer than MTU")
				cs.fail()
				return
			}

			macPos := 2 + dgramLen
			if len(plain) < macPos {
				advanceMAC(len(plain))
				return
			}

			dgramPlain := plain[2:macPos]
			advanceMAC(macPos)
			// TODO: MAC length constant
			endPos := macPos + 32
			if len(plain) < endPos {
				// Don't advance the MAC state past the beginning of the authenticator, but do
				// advance the position.
				cs.inPosition += uint64(len(plain) - macPos)
				return
			}

			authenticated := cs.inMAC.Verify(plain[macPos:endPos])
			cs.inPosition += 32
			cs.inMAC.Reset(cs.inPosition)
			if !authenticated {
				log.Error("datagram failed authentication")
				cs.fail()
				return
			}

			ngram := numberedGram{cs.inSequence, buf.CopyNew(dgramPlain)}
			cs.inSequence++
			select {
			case cs.inGrams <- ngram:
			default:
				// Inward-facing side not consuming these fast enough.  Drop the datagram on
				// the floor.
			}

			consume(endPos)
			beforeCrypt -= 32
			if beforeCrypt < 0 {
				crypt = crypt[-beforeCrypt:]
				beforeCrypt = 0
			}
			if beforeCrypt > 0 {
				panic("Dust/crypting: somehow lost the encrypted bytes needed for next MAC")
			}
		}
	}

	return
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

		if cs.inHandshake != nil {
			n += buf.CopyReassemble(&cs.inHandshake, &p)
			if cs.inHandshake.FixedSizeComplete() {
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
			p0 := p
			subn := buf.TransformReassemble(&cs.inStreaming, &p, cs.inCipher.XORKeyStream)
			n += subn
			consumed := cs.continueUnframing(cs.inStreaming.Data(), cs.inStreaming.ValidLen()-subn, p0[:subn])
			cs.inStreaming.Consume(consumed)
		}
	}

	if len(p) > 0 && err == nil {
		// Oops.
		err = io.ErrShortWrite
	}
	return n, err
}

// Read reads a single datagram into p, blocking if none are available.  The signature is _mostly_ that of
// io.Reader.Read.  TODO: doc rest of signature
func (cs *Session) Read(p []byte) (n int, err error) {
	if log.IsEnabledFor(logging.DEBUG) {
		defer func() {
			log.Debug("  <- %d plain bytes", n)
		}()
	}

	var ngram numberedGram
	if cs.inGramLeft.seq != 0 {
		ngram = cs.inGramLeft
		cs.inGramLeft = numberedGram{}
	} else {
		ngram = <-cs.inGrams
	}

	cs.inLossage = 0
	if ngram.seq == 0 {
		return 0, io.EOF
	} else if ngram.seq != cs.inLast+1 {
		cs.inLossage = ngram.seq - (cs.inLast + 1)
		cs.inLast = ngram.seq - 1
		err = ErrSomeDatagramsLost
	}

	dgram := ngram.data
	n = copy(p, dgram)
	if n < len(dgram) {
		err = io.ErrShortBuffer
	}
	return n, err
}

func (cs *Session) GetReadLossage() int {
	return int(cs.inLossage)
}

// Write queues the datagram p for transmission via cs, blocking if intermediary buffers are full.  The
// signature is that of io.Writer.Write.
func (cs *Session) Write(p []byte) (n int, err error) {
	if log.IsEnabledFor(logging.DEBUG) {
		defer func() {
			log.Debug("-> %d plain bytes", n)
		}()
	}

	dgram := buf.CopyNew(p)
	if len(dgram) > cs.MTU {
		dgram = dgram[:cs.MTU]
		err = io.ErrShortWrite
	}
	cs.outPlains <- buf.CopyNew(dgram)
	return len(dgram), err
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
