// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package shaping

import (
	"time"

	"github.com/blanu/Dust/go/proc"
)

type timer struct {
	proc.Ctl

	maxDuration time.Duration
	useDuration bool
}

func (t *timer) Init(parent *proc.Env) {
	proc.InitDriver(parent, &t.Ctl, t.runTimer, nil)
}

func (t *timer) setDuration(dur time.Duration) {
	t.maxDuration = dur
	t.useDuration = true
}

func (t *timer) runTimer(env *proc.Env) (err error) {
	// TODO: does Go provide access to the monotonic clock?  This is problematic if it's using
	// CLOCK_REALTIME or similar as a base and we have time skew due to NTP etc.
	fixedEndTime := time.Now().Add(t.maxDuration)

	for req, any := env.GetRequest(); any; req, any = env.GetRequest() {
		dur := req.(time.Duration)
		beforeSleep := time.Now()
		reqEndTime := beforeSleep.Add(dur)
		if t.useDuration && reqEndTime.After(fixedEndTime) {
			dur = fixedEndTime.Sub(beforeSleep)
		}

		log.Debug("sleeping for %v", dur)
		time.Sleep(dur)
		afterSleep := time.Now()
		if t.useDuration && afterSleep.After(fixedEndTime) {
			break
		}
		env.PutReply(afterSleep)
	}

	return nil
}

func (t *timer) cycle(dur time.Duration) {
	log.Debug("requesting %v", dur)
	t.PutRequest(dur)
}
