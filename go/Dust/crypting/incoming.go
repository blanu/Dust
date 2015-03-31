package crypting

import (
	"io"

	"github.com/blanu/Dust/go/Dust/buf"
)

// decodeFrames continues decoding frames.  The earliest byte of inStreaming must be in padding or at the
// start of a frame.
func (cs *Session) decodeFrames(crypt []byte, cryptOffset int) (n int) {
	// Invariant: plain starts at absolute position plainStart.  crypt starts at cryptOffset from plain.
	plain := cs.inStreaming.Data()
	plainStart := cs.inFrameStart
	defer func() {
		log.Debug("consuming %d bytes", int(plainStart - cs.inFrameStart))
		cs.inStreaming.Consume(int(plainStart - cs.inFrameStart))
		cs.inFrameStart = plainStart
	}()

	// Ensure the MAC is advanced to at least plainStart+relPos.
	advanceData := func(relPos int) {
		log.Debug("advanceData(%d)", relPos)
		absPos := plainStart + uint64(relPos)
		if cs.inPosition < absPos {
			needed := int(absPos - cs.inPosition)
			if relPos - needed - cryptOffset < 0 {
				panic("Dust/crypting: somehow lost the bytes we needed for the MAC")
			}

			cs.inMAC.Write(crypt[relPos-needed-cryptOffset:relPos-cryptOffset])
			cs.inPosition += uint64(needed)
		}
	}

	// Overwrite plaintext bytes with encrypted bytes between relFrom and relTo, assuming that
	// any bytes earlier than in crypt were already so overwritten.  Do not write them to the MAC
	// or advance the input position.
	advanceNoCipher := func(relFrom, relTo int) {
		log.Debug("advanceNoCipher(%d, %d)", relFrom, relTo)
		if relFrom < cryptOffset {
			relFrom = cryptOffset
		}
		copy(plain[relFrom:relTo], crypt[relFrom-cryptOffset:relTo-cryptOffset])
	}

	// Consume n bytes, advancing plain and crypt.
	consume := func(n int) {
		log.Debug("consume(%d)", n)
		plain = plain[n:]
		if len(plain) >= 2 {
			log.Debug("plain starts with %v", plain[:2])
		}
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
		log.Debug("have %d plains, %d+%d crypts", len(plain), cryptOffset, len(crypt))
		if plain[0] == 0 {
			k := 1
			for k < len(plain) && plain[k] == 0 {
				k++
			}
			advanceData(k)
			consume(k)
		} else if len(plain) < 2 {
			advanceData(len(plain))
			return
		} else {
			tLen := int(uint16(plain[0])<<8 | uint16(plain[1]))
			dgramLen := tLen - 256
			if dgramLen > cs.MTU {
				log.Error("corrupted datagram")
				cs.fail()
				return
			}

			macPos := 2 + dgramLen
			if len(plain) < macPos {
				advanceData(len(plain))
				return
			}

			dgramPlain := plain[2:macPos]
			advanceData(macPos)
			// TODO: MAC length constant
			endPos := macPos + 32
			if len(plain) < endPos {
				advanceNoCipher(macPos, len(plain))
				return
			}

			advanceNoCipher(macPos, endPos)
			authenticated := cs.inMAC.Verify(plain[macPos:endPos])
			cs.inPosition += 32
			cs.inMAC.Reset(cs.inPosition)
			log.Debug("input MAC reset at %d", cs.inPosition)
			if !authenticated {
				log.Error("corrupted datagram")
				cs.fail()
				return
			}

			unsent, unsentOwned := dgramPlain, false

			// We can only try to send directly if there's nothing in line ahead of it.
			if len(cs.pushBuffer) == 0 {
				unsent, unsentOwned = cs.front.PushGram(unsent, false)
			}

			// Only try to hold it for later sending if we're supposed to do that.  Otherwise,
			// drop it on the floor.
			if unsent != nil && cs.HoldIncoming {
				if !unsentOwned {
					unsent = buf.CopyNew(unsent)
				}
				cs.pushBuffer = append(cs.pushBuffer, unsent)
			}

			consume(endPos)
		}
	}
	return
}

func (cs *Session) drainPushBuffer() {
	for len(cs.pushBuffer) > 0 {
		dgramPlain := cs.pushBuffer[0]
		unsent, _ := cs.front.PushGram(dgramPlain, true)
		if unsent != nil {
			return
		}

		cs.pushBuffer = cs.pushBuffer[1:]
	}

	cs.pushBuffer = nil
}

// PushRead processes new pre-decryption bytes in p, handling handshake completions and sending any usable
// plaintext that results to the inward-facing side of the Session.  This method must be called from the
// outward-facing side of the Session.  The signature is similar to io.Writer.Write.
func (cs *Session) PushRead(p []byte) (n int, err error) {
	log.Debug("  <- pushing %d bytes", len(p))
	n, err = 0, nil

	cs.drainPushBuffer()

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
			subn := cs.inStreaming.TransformIn(p, cs.inCipher.XORKeyStream)
			n += subn
			consumed := cs.decodeFrames(p[:subn], cs.inStreaming.ValidLen()-subn)
			cs.inStreaming.Consume(consumed)
			cs.inFrameStart += uint64(consumed)
			p = p[subn:]
		}
	}

	if len(p) > 0 && err == nil {
		// Oops.
		err = io.ErrShortWrite
	}
	return n, err
}

func (cs *Session) PushReadCTS() bool {
	return len(cs.pushBuffer) == 0
}
