// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package proc

import (
	"sync/atomic"
)

type Event struct {
	Val  interface{}
	trig chan<- struct{}
	once uint32
}

func NewEvent() (*Event, <-chan struct{}) {
	ch := make(chan struct{}, 0)
	return &Event{trig: ch}, ch
}

func (ev *Event) Trip(val interface{}) bool {
	if !atomic.CompareAndSwapUint32(&ev.once, 0, 1) {
		return false
	}

	ev.Val = val
	close(ev.trig)
	return true
}

func propagateEvent(fromChan <-chan struct{}, from, to *Event) {
	_ = <-fromChan
	to.Trip(from.Val)
}
