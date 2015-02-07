/*
Package proc holds mainly utilities for managing the lifetime of a goroutine using kill/exit channels.
*/
package proc

import (
	"errors"
	"fmt"
	"os"
)

type PanicError struct {
	Value interface{}
}

func (pe *PanicError) Error() string {
	return fmt.Sprintf("panic: %v", pe.Value)
}

func Debug(control string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, control+"\n", args...)
}

var (
	ErrDoubleFault = errors.New("procman: double fault!")
)

type Link struct {
	heldError  error
	panicAlloc *PanicError
	thunk      func() error
	Req        chan interface{}
	Rep        chan interface{}
	Kill       chan interface{}
	Exit       chan interface{}
}

func drain(ch <-chan interface{}) {
	for _, any := <-ch; any; _, any = <-ch {
		// Loop until channel is closed.
	}
}

func (link *Link) InitLink(thunk func() error) *Link {
	*link = Link{
		thunk:      thunk,
		panicAlloc: &PanicError{nil},
		Req:        make(chan interface{}, 0),
		Rep:        make(chan interface{}, 0),
		Kill:       make(chan interface{}, 0),
		Exit:       make(chan interface{}, 0),
	}

	return link
}

func (link *Link) CloseDetach() {
	if link.Kill == nil {
		return
	}

	close(link.Req)
	close(link.Kill)
	link.Req = nil
	link.Kill = nil
}

func (link *Link) CloseWait() error {
	if link.Kill == nil {
		return link.heldError
	}

	close(link.Req)
	close(link.Kill)
	link.Req = nil
	link.Kill = nil
	drain(link.Exit)
	return link.heldError
}

func (link *Link) PutRequest(req interface{}) bool {
	select {
	case link.Req <- req:
		return true
	case _, _ = <-link.Exit:
		return false
	}
}

func (link *Link) GetReply() (interface{}, bool) {
	rep, ok := <-link.Rep
	return rep, ok
}

func (link *Link) GetRequest() (interface{}, bool) {
	req, any := <-link.Req
	return req, any
}

func (link *Link) PutReply(rep interface{}) bool {
	select {
	case _, _ = <-link.Kill:
		return false
	case link.Rep <- rep:
		return true
	}
}

func (link *Link) reportError(cell *error) {
	err := *cell
	if panicked := recover(); panicked != nil {
		// Clobber any existing error.
		link.panicAlloc.Value = panicked
		err = link.panicAlloc
	}
	link.heldError = err
}

func (link *Link) Spawn() *Link {
	go func() {
		link.heldError = ErrDoubleFault
		defer close(link.Exit)
		defer close(link.Rep)
		var err error
		defer link.reportError(&err)
		err = link.thunk()
	}()

	return link
}
