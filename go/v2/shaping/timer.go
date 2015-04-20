// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package shaping

import (
	"time"

	"github.com/blanu/Dust/go/proc"
)

type timer struct {
	proc.Ctl
}

func (t *timer) Init(parent *proc.Env) {
	proc.InitDriver(parent, &t.Ctl, t.runTimer, nil)
}

func (t *timer) runTimer(env *proc.Env) (err error) {
	for req, any := env.GetRequest(); any; req, any = env.GetRequest() {
		dur := req.(time.Duration)
		log.Debug("            .. sleep %v ..", dur)
		time.Sleep(dur)
		afterSleep := time.Now()
		env.PutReply(afterSleep)
	}

	return nil
}

func (t *timer) cycle(dur time.Duration) {
	t.PutRequest(dur)
}
