// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package shaping

import (
	"io"

	"github.com/blanu/Dust/go/proc"
)

type reader struct {
	proc.Ctl

	readFrom  io.Reader
}

func (r *reader) Init(parent *proc.Env, readFrom io.Reader) {
	r.readFrom = readFrom
	proc.InitDriver(parent, &r.Ctl, r.runReader, nil)
}

func (r *reader) runReader(env *proc.Env) (err error) {
	for req, any := env.GetRequest(); any; req, any = env.GetRequest() {
		// We now own the shared buffer.
		slice := req.([]byte)
		n, err := r.readFrom.Read(slice)
		if err != nil {
			return err
		}

		env.PutReply(n)
	}

	return nil
}

func (r *reader) cycle(p []byte) {
	r.PutRequest(p)
}
