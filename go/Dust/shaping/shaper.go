package shaping

import (
	"io"

	"github.com/op/go-logging"

	"github.com/blanu/Dust/go/Dust/crypting"
	"github.com/blanu/Dust/go/Dust/proc"
)

var log = logging.MustGetLogger("Dust/shaper")

const (
	shaperBufSize = 1024
)

// Params represents globally applicable options for the shaper itself.  The zero value is the default.
type Params struct {
	IgnoreDuration bool
}

func (params *Params) Validate() error {
	return nil
}

// Shaper represents a process mediating between a shaped channel and a Dust crypting session.  It can be
// managed through its proc.Link structure.
type Shaper struct {
	proc.Ctl
	Params

	crypter   *crypting.Session
	shapedIn  io.Reader
	shapedOut io.Writer
	closer    io.Closer

	reader  reader
	decoder Decoder
	inBuf   []byte
	pushBuf []byte

	timer    timer
	encoder  Encoder
	outBuf   []byte
	pullBuf  []byte
	pullMark int
}

func (sh *Shaper) handleRead(subn int) error {
	// We own inBuf until we cycle the reader again.
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

	_, err := sh.shapedOut.Write(sh.outBuf[:outMark])
	return err
}

func (sh *Shaper) runShaper(env *proc.Env) (err error) {
	defer func() {
		if sh.closer != nil {
			sh.closer.Close()
		}
		log.Info("shaper exiting: %v", err)
	}()

	sh.reader.cycle(0)
	sh.timer.cycle(sh.encoder.NextPacketSleep())
	log.Info("shaper starting")

	for {
		select {
		case subn := <-sh.reader.Rep:
			err = sh.handleRead(subn.(int))
			if err != nil {
				return
			}

		case _ = <-sh.timer.Rep:
			err = sh.handleTimer()
			if err != nil {
				return
			}

		case _ = <-env.Cancel:
			return env.ExitCanceled()
		}
	}
}

// NewShaper initializes a new shaper process object for the outward-facing side of crypter, using in/out for
// receiving and sending shaped data and decoder/encoder as the model for this side of the Dust connection.
// The shaper will not be running.  Call Start() on the shaper afterwards to start it in the background; after
// that point, the shaper takes responsibility for closing the closer if it is not nil.
func NewShaper(
	parent *proc.Env,
	crypter *crypting.Session,
	in io.Reader,
	decoder Decoder,
	out io.Writer,
	encoder Encoder,
	closer io.Closer,
	params *Params,
) (*Shaper, error) {
	sh := &Shaper{
		crypter:   crypter,
		shapedIn:  in,
		shapedOut: out,
		closer:    closer,

		// reader is initialized below
		decoder: decoder,
		inBuf:   make([]byte, shaperBufSize),
		pushBuf: make([]byte, shaperBufSize),

		// timer is initialized below
		encoder:  encoder,
		outBuf:   make([]byte, encoder.MaxPacketLength()),
		pullBuf:  make([]byte, shaperBufSize),
		pullMark: 0,
	}

	if params != nil {
		sh.Params = *params
	}

	env := proc.InitChild(parent, &sh.Ctl, sh.runShaper)
	sh.reader.Init(env, sh.shapedIn, sh.inBuf)
	sh.timer.Init(env)
	if !sh.IgnoreDuration {
		sh.timer.setDuration(sh.encoder.WholeStreamDuration())
	}
	return sh, nil
}
