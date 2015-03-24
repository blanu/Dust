package shaping

import (
	"io"

	"github.com/blanu/Dust/go/Dust/proc"
)

type reader struct {
	proc.Ctl

	readFrom  io.Reader
	sharedBuf []byte
}

func (r *reader) Init(parent *proc.Env, readFrom io.Reader, sharedBuf []byte) {
	r.readFrom = readFrom
	r.sharedBuf = sharedBuf
	proc.InitDriver(parent, &r.Ctl, r.runReader, nil)
}

func (r *reader) runReader(env *proc.Env) (err error) {
	for req, any := env.GetRequest(); any; req, any = env.GetRequest() {
		// We now own the shared buffer.
		offset := req.(int)
		n, err := r.readFrom.Read(r.sharedBuf[offset:])
		if err != nil {
			return err
		}

		env.PutReply(n)
	}

	return nil
}

func (r *reader) cycle(offset int) {
	r.PutRequest(offset)
}
