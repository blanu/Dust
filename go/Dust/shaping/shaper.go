package shaping

import (
	"io"
	"time"

	"github.com/op/go-logging"

	"github.com/blanu/Dust/go/Dust/crypting"
	"github.com/blanu/Dust/go/Dust/proc"
)

var log = logging.MustGetLogger("Dust/shaper")

type shaperReader struct {
	proc.Link
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
	//log.Debug("cycling reader")
	sr.PutRequest(offset)
}

type shaperTimer struct {
	proc.Link

	maxDuration time.Duration
}

func newShaperTimer() *shaperTimer {
	st := &shaperTimer{}
	st.InitLink(st.run)
	return st
}

func (st *shaperTimer) setDuration(dur time.Duration) {
	log.Debug("-> expected total duration: %v", dur)
	st.maxDuration = dur
}

func (st *shaperTimer) run() (err error) {
	// TODO: does Go provide access to the monotonic clock?  This is problematic if it's using
	// CLOCK_REALTIME or similar as a base and we have time skew due to NTP etc.
	fixedEndTime := time.Now().Add(st.maxDuration)

	for req, any := st.GetRequest(); any; req, any = st.GetRequest() {
		dur := req.(time.Duration)
		beforeSleep := time.Now()
		reqEndTime := beforeSleep.Add(dur)
		if reqEndTime.After(fixedEndTime) {
			dur = fixedEndTime.Sub(beforeSleep)
		}

		//log.Debug("sleeping for %v", dur)
		time.Sleep(dur)
		afterSleep := time.Now()
		if afterSleep.After(fixedEndTime) {
			break
		}
		st.PutReply(afterSleep)
	}

	return nil
}

func (st *shaperTimer) cycle(dur time.Duration) {
	log.Debug("-> waiting %v", dur)
	st.PutRequest(dur)
}

// Shaper represents a process mediating between a shaped channel and a Dust crypting session.  It can be
// managed through its proc.Link structure.
type Shaper struct {
	proc.Link

	crypter   *crypting.Session
	shapedIn  io.Reader
	shapedOut io.Writer
	closer    io.Closer

	reader  *shaperReader
	decoder Decoder
	inBuf   []byte
	pushBuf []byte

	timer    *shaperTimer
	encoder  Encoder
	outBuf   []byte
	pullBuf  []byte
	pullMark int
}

func (sh *Shaper) handleRead(subn int) error {
	// We own inBuf until we cycle the reader again.
	//log.Debug("read %d bytes", subn)
	in := sh.inBuf[:subn]
	for {
		dn, sn := sh.decoder.UnshapeBytes(sh.pushBuf, in)
		log.Debug("  <- unshaped %d from %d bytes", dn, sn)
		if dn > 0 {
			_, err := sh.crypter.PushRead(sh.pushBuf[:dn])
			if err != nil {
				return err
			}
		}

		in = in[sn:]
		if len(in) == 0 {
			break
		}
	}

	sh.reader.cycle(0)
	return nil
}

func (sh *Shaper) handleTimer() error {
	outLen := int(sh.encoder.NextPacketLength())
	sh.timer.cycle(sh.encoder.NextPacketSleep())

	outMark := 0
	out := sh.outBuf[:outLen]
	for outMark < outLen {
		if sh.pullMark == 0 {
			req := outLen - outMark
			if req >= len(sh.pullBuf)-sh.pullMark {
				req = len(sh.pullBuf) - sh.pullMark
			}

			pulled, err := sh.crypter.PullWrite(sh.pullBuf[sh.pullMark : sh.pullMark+req])
			if err != nil {
				return err
			}

			sh.pullMark += pulled
		}

		dn, sn := sh.encoder.ShapeBytes(out[outMark:], sh.pullBuf[:sh.pullMark])
		log.Debug("-> shaped %d from %d bytes", dn, sn)
		outMark += dn
		copy(sh.pullBuf, sh.pullBuf[sn:sh.pullMark])
		sh.pullMark -= sn
	}

	//log.Debug("writing %d/%d bytes", outMark, outLen)
	_, err := sh.shapedOut.Write(sh.outBuf[:outMark])
	if err != nil {
		return err
	}

	return nil
}

func (sh *Shaper) run() (err error) {
	defer sh.closer.Close()
	defer sh.reader.CloseDetach()
	sh.reader.Spawn()
	defer sh.timer.CloseDetach()
	sh.timer.setDuration(sh.encoder.WholeStreamDuration())
	sh.timer.Spawn()
	sh.reader.cycle(0)
	sh.timer.cycle(sh.encoder.NextPacketSleep())
	defer log.Debug("shaper exiting")

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
// The shaper will not be running.  Call Spawn() on the shaper afterwards to start it in the background; after
// that point, the shaper takes responsibility for delivering a close signal to closer.
func NewShaper(
	crypter *crypting.Session,
	in io.Reader,
	decoder Decoder,
	out io.Writer,
	encoder Encoder,
	closer io.Closer,
) (*Shaper, error) {
	sh := &Shaper{
		crypter:   crypter,
		shapedIn:  in,
		shapedOut: out,
		closer:    closer,

		reader:  nil, // initialized below
		decoder: decoder,
		inBuf:   make([]byte, shaperBufSize),
		pushBuf: make([]byte, shaperBufSize),

		timer:    nil, // initialized below
		encoder:  encoder,
		outBuf:   make([]byte, encoder.MaxPacketLength()),
		pullBuf:  make([]byte, shaperBufSize),
		pullMark: 0,
	}

	sh.InitLink(sh.run)
	sh.reader = newShaperReader(sh.shapedIn, sh.inBuf)
	sh.timer = newShaperTimer()
	return sh, nil
}
