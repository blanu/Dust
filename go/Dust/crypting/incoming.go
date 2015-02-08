package crypting

import (
	"io"

	"github.com/blanu/Dust/go/Dust/buf"
)

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

			cs.front.PushGram(dgramPlain, false)

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
