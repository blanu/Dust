// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package crypting

import (
	"io"
	"sync"
	"time"

	"github.com/blanu/Dust/go/buf"
	"github.com/blanu/Dust/go/prim1"
)

type incoming struct {
	session   *Session
	handshake *handshake

	state     state
	stateChan chan<- state

	// Incoming data must be processed against this first, if set, before cipher.  When HandshakeNoKey:
	// reassembling an ephemeral public key.  When HandshakeKey: searching for the confirmation code from
	// the remote side.
	handshakeReassembly buf.Reassembly

	// Set in HandshakeKey state.
	confirm prim.AuthValue

	// Set in Established state.  In other states, cipher uses a random key.  macPosition refers to the
	// position of the MAC for incoming data; it may be within a frame that is already being reassembled.
	cipher      prim.Cipher
	mac         prim.VerifyingMAC
	macPosition uint64

	// Data is reassembled into frames here.  The reassembly capacity is the maximum frame wire size.
	// frameReassembly starts at frameStart of the stream; this is measured against macPosition to
	// determine how far the MAC has been advanced.
	frameReassembly buf.Reassembly
	frameStart      uint64

	frontGrams     chan []byte
	frontBufs      chan []byte
	frontHeldBuf   []byte
	frontHeldTail  []byte

	readMutex     sync.Mutex
	readInterrupt <-chan struct{}
	readDeadline  time.Time

	writeInterrupt <-chan struct{}
}

func (ic *incoming) Init(session *Session) {
	ic.session = session
	ic.handshake = &session.handshake
	ic.state = stateHandshakeNoKey
	ic.stateChan = session.stateChan
	ic.cipher.SetRandomKey()
	ic.handshakeReassembly = buf.BeginReassembly(prim.PublicBinaryLen)
	ic.frameReassembly = buf.BeginReassembly(session.MTU + frameOverhead)

	nbufs := 3
	ic.frontGrams = make(chan []byte, nbufs)
	ic.frontBufs = make(chan []byte, nbufs)
	for i := 0; i < nbufs; i++ {
		ic.frontBufs <- make([]byte, 0, session.MTU)
	}
	ic.frontHeldBuf = nil
	ic.frontHeldTail = nil
}

func (ic *incoming) receivedEphemeralKey() {
	log.Debug("<-  received ephemeral key")
	ic.handshake.completeWith(ic.handshakeReassembly.Data())

	inKeys := &ic.handshake.in
	ic.cipher.SetKey(inKeys.cipherKey)
	ic.mac.SetKey(inKeys.authKey)
	ic.mac.Reset(0)
	ic.macPosition = 0
	ic.confirm = inKeys.confirm
	log.Debug("<-  expect confirmation starting with %x", ic.confirm[:2])
	inKeys.Clear()

	ic.handshakeReassembly = buf.BeginReassembly(prim.AuthLen)
	ic.state = stateHandshakeKey
	ic.stateChan <- ic.state
}

// Change to the Established state if we've reassembled a correct confirmation code.  Otherwise, throw away
// the chunk and continue.
func (ic *incoming) checkConfirmation() {
	if !ic.confirm.Equal(ic.handshakeReassembly.Data()) {
		ic.handshakeReassembly.Reset()
		return
	}

	// We already set up all the derived keys when we received the ephemeral key.
	log.Debug("<-  ESTABLISHED")
	ic.handshakeReassembly = nil
	ic.frameStart = 0
	ic.state = stateEstablished
	ic.stateChan <- ic.state
}

func (ic *incoming) fail() {
	log.Debug("<-  FAIL")
	ic.handshakeReassembly = nil
	ic.state = stateFailed
	ic.stateChan <- ic.state
}

// decodeFrames continues decoding frames.  The earliest byte of frameReassembly must be in padding or at the
// start of a frame.
func (ic *incoming) decodeFrames(crypt []byte, cryptOffset int) (err error) {
  	// Invariant: plain starts at absolute position plainStart.  crypt starts at cryptOffset from plain.
	plain := ic.frameReassembly.Data()
	plainStart := ic.frameStart
	defer func() {
		//log.Debug("finally consuming %d bytes", int(plainStart - ic.frameStart))
		ic.frameReassembly.Consume(int(plainStart - ic.frameStart))
		ic.frameStart = plainStart
	}()

	// TODO: these subfunction names are kind of confusing.

	// Ensure the MAC is advanced to at least plainStart+relPos.
	advanceData := func(relPos int) {
		//log.Debug("advanceData(%d)", relPos)
		absPos := plainStart + uint64(relPos)
		if ic.macPosition < absPos {
			needed := int(absPos - ic.macPosition)
			//log.Debug("thus writing %d bytes to MAC", needed)
			if relPos - needed - cryptOffset < 0 {
				panic("Dust/crypting: somehow lost the bytes we needed for the MAC")
			}

			ic.mac.Write(crypt[relPos-needed-cryptOffset:relPos-cryptOffset])
			ic.macPosition += uint64(needed)
		}
	}

	// Overwrite plaintext bytes with encrypted bytes between relFrom and relTo, assuming that
	// any bytes earlier than in crypt were already so overwritten.  Do not write them to the MAC
	// or advance the input position.
	advanceNoCipher := func(relFrom, relTo int) {
		//log.Debug("advanceNoCipher(%d, %d)", relFrom, relTo)
		if relFrom < cryptOffset {
			relFrom = cryptOffset
		}
		//log.Debug("thus overwriting %d..%d from %d..%d", relFrom, relTo, relFrom-cryptOffset, relTo-cryptOffset)
		copy(plain[relFrom:relTo], crypt[relFrom-cryptOffset:relTo-cryptOffset])
	}

	// Consume n bytes, advancing plain and crypt.
	consume := func(n int) {
		//log.Debug("consume(%d)", n)
		plain = plain[n:]
		plainStart += uint64(n)
		cryptOffset -= n
		if cryptOffset < 0 {
			crypt = crypt[-cryptOffset:]
			cryptOffset = 0
		}
	}
	
	// Postcondition: as much of crypt as is relevant has already been fed to inMAC, and
	// inPosition has been advanced past all of crypt.

	for len(plain) > 0 {
		//log.Debug("have %d plains, %d+%d crypts", len(plain), cryptOffset, len(crypt))
		if plain[0] == 0 {
			k := 1
			for k < len(plain) && plain[k] == 0 {
				k++
			}
			log.Debug("< - padding len %d", k)
			advanceData(k)
			consume(k)
		} else if len(plain) < 2 {
			advanceData(len(plain))
			return
		} else {
			tLen := int(uint16(plain[0])<<8 | uint16(plain[1]))
			dgramLen := tLen - 256
			//log.Debug("< - possible len %d", dgramLen)
			if dgramLen > ic.session.MTU {
				ic.fail()
				return
			}

			macPos := 2 + dgramLen
			if len(plain) < macPos {
				advanceData(len(plain))
				return
			}

			dgramPlain := plain[2:macPos]
			advanceData(macPos)
			endPos := macPos + prim.AuthLen
			if len(plain) < endPos {
				advanceNoCipher(macPos, len(plain))
				return
			}

			advanceNoCipher(macPos, endPos)

			// We have to pick up the buffer first so that if we get interrupted, we haven't
			// already reset the MAC.
			var frontBuf []byte
			select {
			case frontBuf = <-ic.frontBufs:
			case _ = <-ic.writeInterrupt:
				// FUTURE: make _resuming_ from interruption work.  At the moment we don't
				// keep enough state to resume; we need to remember how many encrypted bytes
				// will be returned unwritten but have actually already been processed into
				// the reassembly buffer, so that we don't append them again.
				log.Debug("<-  write interrupted!")
				ic.fail()
				err = ErrCrashInterrupted
				return
			}

			authenticated := ic.mac.Verify(plain[macPos:endPos])
			ic.macPosition += prim.AuthLen
			ic.mac.Reset(ic.macPosition)
			log.Debug("<-  MAC reset at %d", ic.macPosition)
			if !authenticated {
				ic.frontBufs <- frontBuf
				ic.fail()
				return
			}

			log.Debug("<-- DATA len %d", len(dgramPlain))
			dgramCopy := frontBuf[:copy(frontBuf[:cap(frontBuf)], dgramPlain)]
			ic.frontGrams <- dgramCopy
			consume(endPos)
		}
	}
	return
}

// Write processes new pre-decryption bytes in p, handling handshake completions and sending any usable
// plaintext that results onward.
func (ic *incoming) Write(p []byte) (n int, err error) {
	log.Debug("<   pushing %d bytes", len(p))
	n, err = 0, nil

	for len(p) > 0 {
		if ic.state == stateFailed {
			return n + len(p), err
		}

		if ic.handshakeReassembly != nil {
			n += buf.CopyReassemble(&ic.handshakeReassembly, &p)
			if ic.handshakeReassembly.FixedSizeComplete() {
				switch ic.state {
				default:
					panic("Dust/crypting: handshake reassembly in weird state")
				case stateHandshakeNoKey:
					ic.receivedEphemeralKey()
				case stateHandshakeKey:
					ic.checkConfirmation()
				}
			}
		} else {
			subn :=	ic.frameReassembly.TransformIn(p, ic.cipher.XORKeyStream)
			err = ic.decodeFrames(p[:subn], ic.frameReassembly.ValidLen()-subn)
			n += subn
			p = p[subn:]
		}
	}

	return n, err
}

func (ic *incoming) SetWriteInterrupt(ch <-chan struct{}) error {
	ic.writeInterrupt = ch
	return nil
}

func (ic *incoming) Read(p []byte) (n int, err error) {
	ic.readMutex.Lock()
	defer ic.readMutex.Unlock()

	var timerChan <-chan time.Time
	if !ic.readDeadline.IsZero() {
		timer := time.NewTimer(ic.readDeadline.Sub(time.Now()))
		defer timer.Stop()
		timerChan = timer.C
	}

	for len(p) > 0 {
		if ic.frontHeldTail != nil {
			subn := copy(p, ic.frontHeldTail)
			p = p[subn:]
			ic.frontHeldTail = ic.frontHeldTail[subn:]
			n += subn

			if len(ic.frontHeldTail) == 0 {
				ic.frontBufs <- ic.frontHeldBuf
				ic.frontHeldBuf = nil
				ic.frontHeldTail = nil
			}
		} else {
			var dgram []byte
			if n == 0 {
				select {
				case dgram = <-ic.frontGrams:
				case _ = <-timerChan:
					err = ErrTimeout
					return
				case _ = <-ic.readInterrupt:
					err = ErrInterrupted
					return
				}
			} else {
				select {
				case dgram = <-ic.frontGrams:
				default:
					return
				}
			}

			if dgram == nil {
				err = io.EOF
				return
			}

			subn := copy(p, dgram)
			p = p[subn:]
			if subn == len(dgram) {
				ic.frontBufs <- dgram
			} else {
				ic.frontHeldBuf = dgram
				ic.frontHeldTail = dgram[subn:]
			}
			n += subn
		}
	}

	return
}

func (ic *incoming) SetReadDeadline(t time.Time) error {
	ic.readDeadline = t
	return nil
}

func (ic *incoming) SetReadInterrupt(ch <-chan struct{}) error {
	ic.readInterrupt = ch
	return nil
}
