package shaping

import (
	"io"
	"runtime"

	"github.com/blanu/Dust/go/proc"
)

type interruptibleReader interface {
	io.Reader
	SetReadInterrupt(ch <-chan struct{}) error
}

type outgoing struct {
	encoder Encoder
	visible io.Writer
	uniform interruptibleReader
	timer   timer

	pullBuf []byte
	outBuf  []byte
}

func (og *outgoing) Init(
	parent *proc.Env,
	visible io.Writer,
	uniform interruptibleReader,
	encoder Encoder,
) {
	maxLen := encoder.MaxPacketLength()
	*og = outgoing{
		encoder: encoder,
		visible: visible,
		uniform: uniform,

		pullBuf: make([]byte, 0, maxLen),
		outBuf:  make([]byte, 0, maxLen),
	}
	proc.InitHelper(parent, og.runOutgoing)
	og.timer.Init(parent)
}

func (og *outgoing) issueOneWrite(desiredLen int) error {
	// TODO: expand buf API and use it here
	for len(og.outBuf) < desiredLen {
		if len(og.pullBuf) > 0 {
			dn, sn := og.encoder.ShapeBytes(og.outBuf[len(og.outBuf):desiredLen], og.pullBuf)
			log.Debug("    --] shaped %d from %d bytes", dn, sn)
			og.outBuf = og.outBuf[:len(og.outBuf)+dn]
			og.pullBuf = og.pullBuf[:copy(og.pullBuf, og.pullBuf[sn:])]
		} else {
			// TODO: encoder expansion guesses?
			pullDelta := desiredLen - len(og.outBuf) - len(og.pullBuf)
			if pullDelta < 1 {
				pullDelta = 1
			} else if pullDelta > cap(og.pullBuf) - len(og.pullBuf) {
				pullDelta = cap(og.pullBuf) - len(og.pullBuf)
			}

			log.Debug("    - ] pulling up to %d bytes", pullDelta)
			rn, err := og.uniform.Read(og.pullBuf[len(og.pullBuf):len(og.pullBuf)+pullDelta])
			if err != nil {
				return err
			}
			log.Debug("     -] pulled %d bytes", rn)
			og.pullBuf = og.pullBuf[:len(og.pullBuf)+rn]
		}
	}

	log.Debug("      ] visible: %d bytes", desiredLen)
	wn, err := og.visible.Write(og.outBuf[:desiredLen])
	if err != nil {
		log.Debug("      ] write: %d/%d, %v", wn, desiredLen, err)
	}
	og.outBuf = og.outBuf[:copy(og.outBuf, og.outBuf[wn:])]
	return err
}

func (og *outgoing) runOutgoing(env *proc.Env) (err error) {
	og.timer.cycle(og.encoder.NextPacketSleep())
	err = og.uniform.SetReadInterrupt(env.Cancel)
	if err != nil {
		return
	}
	
	for {
		select {
		case _ = <-env.Cancel:
			return env.ExitCanceled()
		case _, ok := <-og.timer.Rep:
			if !ok {
				return
			}

			og.timer.cycle(og.encoder.NextPacketSleep())
			err = og.issueOneWrite(int(og.encoder.NextPacketLength()))
			if err != nil {
				return
			}

			// This is a kludge to try to avoid starving other goroutines in the event that our
			// model schedules a large number of writes within a short period of time.  In
			// particular, we seem to potentially starve the incoming goroutine in that case,
			// yielding significant performance degradation and stuffing the output stream full of
			// expensive padding.
			runtime.Gosched()
		}
	}
}
