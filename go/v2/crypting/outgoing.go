// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package crypting

import (
	"sync"
	"time"

	"github.com/blanu/Dust/go/buf"
	"github.com/blanu/Dust/go/prim1"
)

type outgoing struct {
	session   *Session
	handshake *handshake

	state     state
	stateChan <-chan state

	// Implicitly prepended to any other outgoing data; these bytes are not additionally encrypted before
	// sending.
	queued buf.Reassembly

	// Set in Streaming state.  In other states, cipher uses a random key.
	cipher      prim.Cipher
	mac         prim.GeneratingMAC
	macPosition uint64

	// Scratch space for advancing the stream cipher when copying MAC bytes.
	macGarbage [prim.AuthLen]byte

	// Datagrams from the client application are pushed to frontGrams, using buffers cycled through frontBufs.
	// TODO: interrupt support
	frontGrams  chan []byte
	frontBufs   chan []byte

	writeMutex     sync.Mutex
	writeInterrupt <-chan struct{}
	writeDeadline  time.Time
}

func (og *outgoing) Init(session *Session, initialData []byte) {
	og.session = session
	og.handshake = &session.handshake
	og.state = stateHandshakeNoKey
	og.stateChan = session.stateChan
	og.cipher.SetRandomKey()
	og.queued = buf.BeginReassembly(prim.PublicBinaryLen + 2*prim.AuthLen + session.MTU + frameOverhead)
	og.queued.CopyIn(initialData)

	nbufs := 3
	og.frontGrams = make(chan []byte, nbufs)
	og.frontBufs = make(chan []byte, nbufs)
	for i := 0; i < nbufs; i++ {
		og.frontBufs <- make([]byte, 0, session.MTU)
	}
}

func (og *outgoing) drainFront() {
	for {
		select {
		case dgram := <-og.frontGrams:
			og.frontBufs <- dgram[:0]
		default:
			return
		}
	}
}

func (og *outgoing) pullZeroRaw(p []byte) {
	buf.Zero(p)
	og.cipher.XORKeyStream(p, p)
}

func (og *outgoing) pullZeroData(p []byte) {
	og.pullZeroRaw(p)
	og.mac.Write(p)
	og.macPosition += uint64(len(p))
}

func (og *outgoing) pullData(p []byte) {
	// Should have nothing in outCrypted to start with.
	for len(p) > 0 {
		if og.queued.ValidLen() > 0 {
			panic("Dust/crypting: pullData with leftover crumbs in construction buffer")
		}

		var dgram []byte
		var any bool
		select {
		case dgram, any = <-og.frontGrams:
		default:
		}

		if !any {
			log.Debug("    --> padding len %d", len(p))
			og.pullZeroData(p)
			return
		}
		log.Debug("    --> DATA len %d", len(dgram))

		tLen := 256 + len(dgram)
		header := []byte{uint8(tLen >> 8), uint8(tLen & 0xff)}
		headerN := og.queued.TransformIn(header, og.cipher.XORKeyStream)
		dgramN := og.queued.TransformIn(dgram, og.cipher.XORKeyStream)
		og.mac.Write(og.queued.Data())
		mac := og.mac.Generate()
		macN := og.queued.CopyIn(mac[:])
		// Advance the key stream by the MAC length even though we weren't XORing it while copying the
		// MAC.
		og.cipher.XORKeyStream(og.macGarbage[:], og.macGarbage[:])
		if !(headerN == len(header) && dgramN == len(dgram) && macN == len(mac)) {
			panic("Dust/crypting: somehow not enough space in output buffer")
		}

		og.macPosition += uint64(headerN + dgramN + macN)
		og.mac.Reset(og.macPosition)
		log.Debug("     -> MAC reset at %d", og.macPosition)
		og.queued.CopyOut(&p)
		og.frontBufs <- dgram[:0]
	}
}

func (og *outgoing) handleStateChange() {
	select {
	default:
	case newState := <-og.stateChan:
		switch newState {
		case stateHandshakeKey:
			outKeys := &og.handshake.out
			log.Debug("     -> send confirmation starting with %x", outKeys.confirm[:2])
			og.queued.CopyIn(outKeys.confirm[:])
			og.cipher.SetKey(outKeys.cipherKey)
			og.mac.SetKey(outKeys.authKey)
			og.macPosition = 0
			og.mac.Reset(0)
			outKeys.Clear()

		case stateEstablished:
			// Don't need to do anything; we already set up all the derived keys earlier.

		case stateFailed:
			og.queued = nil
			og.cipher.SetRandomKey()
		}

		og.state = newState
	}
	
}

// Read tries to pull post-encryption bytes into p, using in-band framing to intersperse any plain data
// available to write with enough padding frames to completely fill p immediately.
func (og *outgoing) Read(p []byte) (n int, err error) {
	log.Debug("      > pulling %d bytes", len(p))
	og.handleStateChange()

	n, err = len(p), nil
	if og.queued.ValidLen() > 0 {
		og.queued.CopyOut(&p)
	}

	if len(p) == 0 {
		// This can be true from before the possible CopyOut above, too, so don't move it inside
		// the above conditional.
		return
	}

	switch og.state {
	default:
		panic("Dust/crypting: unhandled state!")

	case stateHandshakeNoKey:
		// Handshake interstitial data must be aligned prim.AuthLen-byte chunks.
		og.pullZeroRaw(p)

		if len(p)%prim.AuthLen != 0 {
			og.pullZeroRaw(og.queued.PreData(prim.AuthLen - len(p)%prim.AuthLen))
		}

		log.Debug("     -> interstitial %d bytes", len(p))

	case stateFailed:
		og.drainFront()
		og.pullZeroRaw(p)

	case stateHandshakeKey:
		// Remote confirmation code hasn't been received yet.  Zeros are valid filler.
		og.pullZeroData(p)

	case stateEstablished:
		// Can send data.
		og.pullData(p)
	}

	return
}

func (og *outgoing) SetReadInterrupt(ch <-chan struct{}) error {
	// Reads never block anyway.
	return nil
}

func (og *outgoing) Write(p []byte) (n int, err error) {
	og.writeMutex.Lock()
	defer og.writeMutex.Unlock()

	var timerChan <-chan time.Time
	if !og.writeDeadline.IsZero() {
		timer := time.NewTimer(og.writeDeadline.Sub(time.Now()))
		defer timer.Stop()
		timerChan = timer.C
	}

	for len(p) > 0 {
		var buf []byte
		select {
		case buf = <-og.frontBufs:
		case _ = <-timerChan:
			err = ErrTimeout
			return
		case _ = <-og.writeInterrupt:
			err = ErrInterrupted
			return
		}

		subn := copy(buf[:cap(buf)], p)
		og.frontGrams <- buf[:subn]
		p = p[subn:]
		n += subn
	}

	return
}

func (og *outgoing) SetWriteDeadline(t time.Time) error {
	og.writeDeadline = t
	return nil
}

func (og *outgoing) SetWriteInterrupt(ch <-chan struct{}) error {
	og.writeInterrupt = ch
	return nil
}
