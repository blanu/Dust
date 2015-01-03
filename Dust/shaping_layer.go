package Dust

import (
	"io"
	"time"
)

type shaperReader struct {
	recycleChan chan int
	advanceChan chan int
	heldError error
	readFrom io.Reader
	sharedBuf []byte
}

const (
	shaperBufSize = 1024
)

func newShaperReader(readFrom io.Reader, sharedBuf []byte) *shaperReader {
	return &shaperReader{
		// These have to be unbuffered for memory-model guarantees.
		recycleChan: make(chan int, 0),
		advanceChan: make(chan int, 0),
		heldError: nil,
		readFrom: readFrom,
		sharedBuf: sharedBuf,
	}
}

func (sr *shaperReader) run() {
	defer func() {
		ReportExitTo(&sr.heldError)
		close(sr.advanceChan)
	}()

	for {
		offset, ok := <-sr.recycleChan
		if !ok {
			return
		}
		// We now own the shared buffer.

		n, err := sr.readFrom.Read(sr.sharedBuf[offset:])
		if err != nil {
			sr.heldError = err
			return
		}

		sr.advanceChan <- n
	}
}

func (sr *shaperReader) cycle(offset int) {
	sr.recycleChan <- offset
}

func (sr *shaperReader) stop() {
	close(sr.recycleChan)
}

type shaperTimer struct {
	durationChan chan time.Duration
	timingChan chan time.Time
	heldError error
}

func newShaperTimer() *shaperTimer {
	return &shaperTimer{
		durationChan: make(chan time.Duration, 1),
		timingChan: make(chan time.Time, 1),
	}
}

func (st *shaperTimer) run() {
	defer func() {
		ReportExitTo(&st.heldError)
		close(st.timingChan)
	}()
	
	st.timingChan <- time.Now()
	for {
		dur, ok := <-st.durationChan
		if !ok {
			return
		}
			
		time.Sleep(dur)
		st.timingChan <- time.Now()
	}
}

func (st *shaperTimer) cycle(dur time.Duration) {
	st.durationChan <- dur
}

func (st *shaperTimer) stop() {
	close(st.durationChan)
}

// INCOMPLETE: this uses a trivial, hardcoded model rather than any actual shaping model

type Shaper struct {
	crypto *CryptoSession
	shapedIn io.Reader
	shapedOut io.Writer
	heldError error

	reader *shaperReader
	inBuf []byte
	inPos int
	
	timer *shaperTimer
	outBuf []byte
	
	controlChan chan int
	statusChan chan int
}

func (sh *Shaper) handleRead(subn int) error {
	// TODO: don't mix up the shaping with the reader.  When we get real models this will go away.
	//
	// We own inBuf until 
	sh.inPos += subn
	if sh.inPos < len(sh.inBuf) {
		sh.reader.cycle(sh.inPos)
		return nil
	}
	
	valid := uint16(sh.inBuf[0]) << 8 | uint16(sh.inBuf[1])
	_, err := sh.crypto.PushRead(sh.inBuf[2:2+valid])
	if err != nil {
		return err
	}

	sh.reader.cycle(0)
	return nil
}

func (sh *Shaper) handleTimer() error {
	sh.timer.cycle(1000 * time.Millisecond)

	n, err := sh.crypto.PullWrite(sh.outBuf[2:])
	if err != nil && err != io.ErrNoProgress {
		return err
	}

	// Normally, n should be exactly what we requested, but it might not be.  When we have actual models
	// we need to handle the discrepancy better (e.g., by fiddling the crypto layer so that it _really is_
	// always exactly what we requested---but that seems to require protocol changes just now).

	if n > 65535 {
		panic("weird n value")
	}
	sh.outBuf[0] = uint8(n >> 8)
	sh.outBuf[1] = uint8(n & 0xff)
	
	n, err = sh.shapedOut.Write(sh.outBuf[:])
	if err != nil {
		return err
	}

	return nil
}

func (sh *Shaper) handleOneStateChange() bool {
	var err error
	select {
	case subn, ok := <-sh.reader.advanceChan:
		if !ok {
			// Reader is dead.
			return true
		}
		
		err = sh.handleRead(subn)
		if err != nil {
			sh.heldError = err
			return true
		}
		
	case _, ok := <-sh.timer.timingChan:
		if !ok {
			// Timer is dead.
			return true
		}

		err = sh.handleTimer()
		if err != nil {
			sh.heldError = err
			return true
		}

	case _, _ = <-sh.controlChan:
		// Closing the control channel is an exit request.
		return true
	}

	return false
}

func (sh *Shaper) run(afterThunk func()) {
	defer func() {
		ReportExitTo(&sh.heldError)
		sh.reader.stop()
		sh.timer.stop()
		close(sh.statusChan)
		afterThunk()
	}()

	go sh.reader.run()
	go sh.timer.run()

	sh.inPos = 0
	sh.reader.cycle(sh.inPos)

	for {
		shouldExit := sh.handleOneStateChange()
		if shouldExit {
			break
		}
	}
}

func NewShaper(crypto *CryptoSession, in io.Reader, out io.Writer) (*Shaper, error) {
	sh := &Shaper{
		crypto: crypto,
		shapedIn: in,
		shapedOut: out,

		inBuf: make([]byte, shaperBufSize),

		outBuf: make([]byte, shaperBufSize),

		heldError: nil,
		controlChan: make(chan int, 1),
		statusChan: make(chan int, 1),
	}

	sh.reader = newShaperReader(sh.shapedIn, sh.inBuf)
	sh.timer = newShaperTimer()
	return sh, nil
}

func (sh *Shaper) SpawnThen(afterThunk func()) {
	go sh.run(afterThunk)
}

func (sh *Shaper) Error() error {
	return sh.heldError
}
