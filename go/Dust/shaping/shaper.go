package shaping

import (
	"io"
	"time"

	"github.com/blanu/Dust/go/Dust/bufman"
	"github.com/blanu/Dust/go/Dust/crypting"
	"github.com/blanu/Dust/go/Dust/procman"
)

type shaperReader struct {
	procman.Link
	readFrom  io.Reader
	sharedBuf []byte
}

const (
	shaperBufSize = 1024
)

func newShaperReader(readFrom io.Reader, sharedBuf []byte) *shaperReader {
	sr := &shaperReader{
		readFrom:  readFrom,
		sharedBuf: sharedBuf,
	}
	sr.InitLink(sr.run)
	return sr
}

func (sr *shaperReader) run() (err error) {
	for req, any := sr.GetRequest(); any; req, any = sr.GetRequest() {
		// We now own the shared buffer.
		offset := req.(int)
		n, err := sr.readFrom.Read(sr.sharedBuf[offset:])
		if err != nil {
			return err
		}

		sr.PutReply(n)
	}

	return nil
}

func (sr *shaperReader) cycle(offset int) {
	sr.PutRequest(offset)
}

type shaperTimer struct {
	procman.Link
}

func newShaperTimer() *shaperTimer {
	st := &shaperTimer{}
	st.InitLink(st.run)
	return st
}

func (st *shaperTimer) run() (err error) {
	st.PutReply(time.Now())
	for req, any := st.GetRequest(); any; req, any = st.GetRequest() {
		dur := req.(time.Duration)
		time.Sleep(dur)
		st.PutReply(time.Now())
	}

	return nil
}

func (st *shaperTimer) cycle(dur time.Duration) {
	st.PutRequest(dur)
}

// Shaper represents a process mediating between a shaped channel and a Dust crypting session.  It can be
// managed through its procman.Link structure.
type Shaper struct {
	procman.Link

	crypter   *crypting.Session
	shapedIn  io.Reader
	shapedOut io.Writer

	reader  *shaperReader
	decoder Decoder
	inBuf   []byte

	timer      *shaperTimer
	encoder    Encoder
	outBuf     []byte
	outPending []byte
	pullBuf    []byte
}

func (sh *Shaper) handleRead(subn int) error {
	// We own inBuf until we cycle the reader again.
	decoded := sh.decoder.UnshapeBytes(sh.inBuf[:subn])
	_, err := sh.crypter.PushRead(decoded)
	if err != nil {
		return err
	}

	sh.reader.cycle(0)
	return nil
}

func (sh *Shaper) handleTimer() error {
	outLen := sh.encoder.NextPacketLength()
	sh.timer.cycle(sh.encoder.NextPacketSleep())

	outValid := 0
	outTail := sh.outBuf[:outLen]
	for len(outTail) > 0 {
		if len(sh.outPending) > 0 {
			outValid += bufman.CopyAdvance(&outTail, &sh.outPending)
			continue
		}

		pullN, err := sh.crypter.PullWrite(sh.pullBuf)
		if err != nil && err != crypting.ErrStuck {
			return err
		}
		encodedTail := sh.encoder.ShapeBytes(sh.pullBuf[:pullN])
		outValid += bufman.CopyAdvance(&outTail, &encodedTail)
		if len(encodedTail) > 0 {
			sh.outPending = append(sh.outPending, encodedTail...)
		}

		if err != nil {
			// It was an ErrStuck, otherwise we'd have returned above.
			break
		}
	}

	_, err := sh.shapedOut.Write(sh.outBuf[:outValid])
	if err != nil {
		return err
	}

	return nil
}

func (sh *Shaper) run() (err error) {
	defer sh.reader.CloseDetach()
	sh.reader.Spawn()
	defer sh.timer.CloseDetach()
	sh.timer.Spawn()
	sh.reader.cycle(0)

	for {
		select {
		case subn, ok := <-sh.reader.Rep:
			if !ok {
				// Reader is dead.
				return sh.reader.CloseWait()
			}

			err = sh.handleRead(subn.(int))
			if err != nil {
				return
			}

		case _, ok := <-sh.timer.Rep:
			if !ok {
				// Timer is dead.
				return sh.timer.CloseWait()
			}

			err = sh.handleTimer()
			if err != nil {
				return
			}

		case _, _ = <-sh.Link.Kill:
			return nil
		}
	}
}

// NewShaper initializes a new shaper process object for the outward-facing side of crypter, using in/out for
// receiving and sending shaped data and decoder/encoder as the model for this side of the Dust connection.
// The shaper will not be running.  Call Spawn() on the Shaper afterwards to start it in the background.
func NewShaper(
	crypter *crypting.Session,
	in io.Reader,
	decoder Decoder,
	out io.Writer,
	encoder Encoder,
) (*Shaper, error) {
	// INCOMPLETE: does not handle connection duration.

	sh := &Shaper{
		crypter:   crypter,
		shapedIn:  in,
		shapedOut: out,

		reader:  nil, // initialized below
		decoder: decoder,
		inBuf:   make([]byte, shaperBufSize),

		timer:      nil, // initialized below
		encoder:    encoder,
		outBuf:     make([]byte, encoder.MaxPacketLength()),
		outPending: nil,
		pullBuf:    make([]byte, shaperBufSize),
	}

	sh.InitLink(sh.run)
	sh.reader = newShaperReader(sh.shapedIn, sh.inBuf)
	sh.timer = newShaperTimer()
	return sh, nil
}
