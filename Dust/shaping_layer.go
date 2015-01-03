package Dust

import (
	"fmt"
	"io"
	"os"
	"time"
)

// INCOMPLETE: this uses a trivial, hardcoded model rather than any actual shaping model

type shaper struct {
	crypto *CryptoSession
	shapedIn io.Reader
	shapedOut io.Writer
}

// TODO: this shouldn't be here.
func exitOnPanic(prefix string) {
	if err := recover(); err != nil {
		fmt.Fprintf(os.Stderr, "panic in %s: %v\n", prefix, err)
		os.Exit(255)
	}
}

func (sh *shaper) run() {
	inSuccessChan := make(chan int, 0)
	inRecycleChan := make(chan int, 0)
	inBuf := make([]byte, 1024)
	inPos := 0

	outTimingChan := make(chan time.Time, 0)
	outDurationChan := make(chan time.Duration, 1)
	outBuf := make([]byte, 1024)

	go func() {
		defer exitOnPanic("reader")
		for {
			offset, ok := <-inRecycleChan
			if !ok {
				break
			}
			dst := inBuf[offset:]
			infof("<- attempting to read %d shaped bytes", len(dst))
			n, err := sh.shapedIn.Read(dst)
			if err != nil {
				infof("aieee: %v", err)
				//panic("handle I/O condition")
				return
			}

			inSuccessChan <- n
		}

		infof("out of reader")
		close(inSuccessChan)
	}()

	go func() {
		defer exitOnPanic("timer")
		outTimingChan <- time.Now()
		for {
			dur, ok := <-outDurationChan
			if !ok {
				break
			}
			infof("sleeping for %d", dur / time.Millisecond)
			time.Sleep(dur)
			outTimingChan <- time.Now()
		}

		infof("out of timer")
		close(outTimingChan)
	}()

	inRecycleChan <- 0
	for {
		//defer exitOnPanic("slinger")
		select {
		case subn, _ := <-inSuccessChan:
			// TODO: handle closed channel
			infof("<- read %d shaped bytes", subn)
			inPos += subn
			if inPos < len(inBuf) {
				inRecycleChan <- inPos
				continue
			}
			
			valid := uint16(inBuf[0]) << 8 | uint16(inBuf[1])
			infof("<- passing along %d crypto bytes", valid)
			_, err := sh.crypto.PushRead(inBuf[2:2+valid])
			if err != nil {
				infof("aieee: %v", err)
				panic("handle I/O condition")
			}
			inRecycleChan <- 0
			
		case _, _ = <-outTimingChan:
			// TODO: handle closed channel
			outDurationChan <- 1000 * time.Millisecond
			n, err := sh.crypto.PullWrite(outBuf[2:])
			if err != nil && err != io.ErrNoProgress {
				infof("aieee: %v", err)
				panic("handle I/O condition")
			}
			infof("-> have %d crypto bytes", n)

			if n > 65535 {
				panic("weird n value")
			}
			outBuf[0] = uint8(n >> 8)
			outBuf[1] = uint8(n & 0xff)
			n, err = sh.shapedOut.Write(outBuf)
			if err != nil {
				infof("aieee: %v", err)
				panic("handle I/O condition")
			}
			infof("-> wrote %d shaped bytes", n)
		}
	}
}

func SpawnShaper(crypto *CryptoSession, rw io.ReadWriter) (*shaper, error) {
	// TODO: put back control channels
	sh := &shaper{crypto, rw, rw}
	go sh.run()
	return sh, nil
}
