// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package shaping

import (
	"io"

	"github.com/op/go-logging"

	"github.com/blanu/Dust/go/v2/crypting"
	"github.com/blanu/Dust/go/proc"
)

var log = logging.MustGetLogger("Dust/shaping")

// Shaper represents a process mediating between a shaped channel and a Dust crypting session.  It can be
// managed through its proc.Link structure.
type Shaper struct {
	proc.Ctl

	incoming incoming
	outgoing outgoing
}

func (sh *Shaper) runShaper(env *proc.Env) (err error) {
	select {
	case _ = <-env.Cancel:
		return env.ExitCanceled()
	}
}

// NewShaper initializes a new shaper process object for the outward-facing side of crypter, using in/out for
// receiving and sending shaped data and decoder/encoder as the model for this side of the Dust connection.
// The shaper will not be running.  Call Start() on the shaper afterwards to start it in the background.
func NewShaper(
	parent *proc.Env,
	crypter *crypting.Session,
	in io.Reader,
	decoder Decoder,
	out io.Writer,
	encoder Encoder,
) (*Shaper, error) {
	sh := &Shaper{}
	env := proc.InitChild(parent, &sh.Ctl, sh.runShaper)
	sh.incoming.Init(env, crypter.Back, in, decoder)
	sh.outgoing.Init(env, out, crypter.Back, encoder)
	return sh, nil
}
