// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

/*
Package proc allows the organization of goroutines into process trees.

TODO: rest of docstrings here
*/
package proc

import (
	"fmt"
	"sync/atomic"
)

type DisplayId uint64

var nextIdStorage uint64

func nextId() DisplayId {
	return DisplayId(atomic.AddUint64(&nextIdStorage, uint64(1)))
}

func (id DisplayId) String() string {
	return fmt.Sprintf("#%d", uint64(id))
}

type Proc struct {
	DisplayId DisplayId

	thunk     func(*Env) error
	interrupt func() error
	onExit    func(error)

	start, cancel, exit *Event

	req, rep chan interface{}
}

type ExitHow int

const (
	ExitNormal ExitHow = iota
	ExitError
	ExitCanceled
)

func (how ExitHow) String() string {
	switch how {
	case ExitNormal, ExitError:
		return "exited"
	case ExitCanceled:
		return "canceled"
	default:
		return "???"
	}
}

type Exited struct {
	How    ExitHow
	Proc   *Proc
	Reason error
}

func (exi Exited) String() string {
	str := fmt.Sprintf("%v(%v)", exi.How, exi.Proc.DisplayId)
	if exi.Reason != nil && exi.Reason != exi {
		str += ": " + exi.Reason.Error()
	}
	return str
}

func (exi Exited) Error() string {
	return exi.String()
}

type Ctl struct {
	*Proc
	Exit <-chan struct{}
	Rep  <-chan interface{}
}

type Env struct {
	*Proc
	Cancel <-chan struct{}
	Req    <-chan interface{}
	Start  <-chan struct{}
}

// Start starts the process under control, including all of its driver and helper processes.
func (ctl *Ctl) Start() {
	ctl.start.Trip(true)
}

// Cancel cancels the process under control, including all of its driver, helper, and child processes.
func (ctl *Ctl) Cancel() {
	ctl.cancel.Trip(&Exited{ExitCanceled, ctl.Proc, nil})
	ctl.start.Trip(false)
	if ctl.interrupt != nil {
		ctl.interrupt()
	}
}

func (ctl *Ctl) Status() *Exited {
	select {
	case _ = <-ctl.Exit:
		return ctl.exit.Val.(*Exited)
	default:
		return nil
	}
}

// ExitCanceled should be called immediately when the env.Cancel channel becomes ready.  It uses a panic
// captured by the proc package to terminate the goroutine.  It must not be called if the env.Cancel channel
// has not become ready.  It never returns, but by convention, one may tail-call it.
func (env *Env) ExitCanceled() error {
	panic(env.Proc.cancel.Val.(*Exited))
	return nil
}

func (env *Env) CancellationPoint() error {
	select {
	case _ = <-env.Cancel:
		return env.ExitCanceled()
	default:
		return nil
	}
}

func (proc *Proc) initNoLink(env *Env) {
	env.Proc = proc
	proc.start, env.Start = NewEvent()
	proc.cancel, env.Cancel = NewEvent()
}

func (proc *Proc) initWeakLink(parent *Env, env *Env) {
	env.Proc = proc
	proc.start, env.Start = NewEvent()
	proc.cancel, env.Cancel = NewEvent()
	if parent != nil {
		go propagateEvent(parent.Cancel, parent.cancel, proc.cancel)
	}
}

func (proc *Proc) initStrongLink(parent *Env, env *Env) {
	env.Proc = proc
	proc.start, env.Start = parent.start, parent.Start
	proc.cancel, env.Cancel = parent.cancel, parent.Cancel
}

func (proc *Proc) initData(ctl *Ctl, env *Env) {
	proc.req = make(chan interface{}, 1)
	env.Req = proc.req
	proc.rep = make(chan interface{}, 1)
	ctl.Rep = proc.rep
}

func (proc *Proc) handleExit(cell *error) {
	var status *Exited
	switch caught := recover().(type) {
	case nil:
		if *cell == nil {
			status = &Exited{ExitNormal, proc, nil}
		} else {
			status = &Exited{ExitError, proc, *cell}
		}
	case *Exited:
		status = caught
	default:
		panic(caught)
	}

	proc.exit.Trip(status)
	proc.cancel.Trip(status)
}

func (proc *Proc) runManaged(env *Env) (err error) {
	defer proc.handleExit(&err)
	_ = <-env.Start
	ok := env.start.Val.(bool)
	if !ok {
		return env.ExitCanceled()
	}
	return proc.thunk(env)
}

func newProc(ctl *Ctl, run func(*Env) error) (proc *Proc, env *Env) {
	proc = &Proc{DisplayId: nextId()}
	env = &Env{Proc: proc}
	proc.thunk = run
	ctl.Proc = proc
	proc.exit, ctl.Exit = NewEvent()
	return
}

func InitDriver(parent *Env, ctl *Ctl, run func(*Env) error, interrupt func() error) (env *Env) {
	proc, env := newProc(ctl, run)
	proc.interrupt = interrupt
	proc.initStrongLink(parent, env)
	proc.initData(ctl, env)
	go proc.runManaged(env)
	return
}

func InitHelper(parent *Env, run func(*Env) error) (env *Env) {
	proc, env := newProc(&Ctl{}, run)
	proc.initStrongLink(parent, env)
	go proc.runManaged(env)
	return
}

func InitChild(parent *Env, ctl *Ctl, run func(*Env) error) (env *Env) {
	proc, env := newProc(ctl, run)
	proc.initWeakLink(parent, env)
	go proc.runManaged(env)
	return
}
