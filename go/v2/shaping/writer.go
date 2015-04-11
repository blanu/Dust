// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package shaping

import (
	"io"

	"github.com/blanu/Dust/go/proc"
)

type writer struct {
	proc.Ctl

	writeTo io.Writer
}

func (w *writer) Init(parent *proc.Env, writeTo io.Writer) {
	w.writeTo = writeTo
	proc.InitDriver(parent, &w.Ctl, w.runWriter, nil)
}

func (w *writer) runWriter(env *proc.Env) (err error) {
	for req, any := env.GetRequest(); any; req, any = env.GetRequest() {
		// We now own the shared buffer.
		slice := req.([]byte)
		n, err := w.writeTo.Write(slice)
		if err != nil {
			return err
		}

		env.PutReply(n)
	}

	return nil
}

func (w *writer) cycle(p []byte) {
	w.PutRequest(p)
}

