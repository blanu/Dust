package crypting

import (
	"github.com/blanu/Dust/go/Dust/buf"
)

func (cs *Session) pullZeroRaw(p []byte) {
	for i := 0; i < len(p); i++ {
		p[i] = 0
	}
	cs.outCipher.XORKeyStream(p, p)
}

func (cs *Session) pullZeroData(p []byte) {
	cs.pullZeroRaw(p)
	cs.outMAC.Write(p)
	cs.outPosition += uint64(len(p))
}

func (cs *Session) pullData(p []byte) {
	// Should have nothing in outCrypted to start with.
	for len(p) > 0 {
		if cs.outCrypted.ValidLen() > 0 {
			panic("Dust/crypting: pullData with leftover crumbs in construction buffer")
		}

		var dgram []byte
		setDgram := func(p []byte, mayRetain bool) {
			if len(p) > cs.MTU {
				p = p[:cs.MTU]
			}

			if mayRetain {
				dgram = p
			} else {
				dgram = buf.CopyNew(p)
			}
		}

		log.Debug("pull gram")
		cs.front.PullGram(setDgram)
		if dgram == nil {
			log.Debug("no gram; write %d zeros", len(p))
			cs.pullZeroData(p)
			return
		}
		log.Debug("gram len %d", len(dgram))

		tLen := 256 + len(dgram)
		header := []byte{uint8(tLen >> 8), uint8(tLen & 0xff)}
		headerN := cs.outCrypted.TransformIn(header, cs.outCipher.XORKeyStream)
		dgramN := cs.outCrypted.TransformIn(dgram, cs.outCipher.XORKeyStream)
		cs.outMAC.Write(cs.outCrypted.Data())
		mac := cs.outMAC.Generate()
		macN := cs.outCrypted.CopyIn(mac[:])
		// Advance the key stream by the MAC length even though we weren't XORing it while copying the
		// MAC.
		cs.outCipher.XORKeyStream(cs.outGarbage, cs.outGarbage)
		if !(headerN == len(header) && dgramN == len(dgram) && macN == len(mac)) {
			panic("Dust/crypting: somehow not enough space in output buffer")
		}

		cs.outPosition += uint64(headerN + dgramN + macN)
		cs.outMAC.Reset(cs.outPosition)
		log.Debug("output MAC reset at %d", cs.outPosition)
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
		cs.pullZeroRaw(p)

		if len(p)%32 != 0 {
			cs.pullZeroRaw(cs.outCrypted.PreData(32 - len(p)%32))
		}

		log.Debug("-> handshake interstitial %d bytes", len(p))

	case stateFailed:
		cs.front.DrainOutput()
		cs.pullZeroRaw(p)

	case stateHandshakeKey:
		// Remote confirmation code hasn't been received yet.  Zeros are valid filler.
		cs.pullZeroData(p)

	case stateStreaming:
		// Can send data.
		cs.pullData(p)
	}

	return
}
