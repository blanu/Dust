// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package proc

// PutRequest sends req to the controlled process's request channel.  It returns true if the request was
// delivered to the channel, or false if the process exited before it could be delivered.
func (ctl *Ctl) PutRequest(req interface{}) bool {
	select {
	case _ = <-ctl.Exit:
		return false
	case ctl.req <- req:
		return true
	}
}

// GetReply receives a reply from the controlled process's reply channel.  It returns the reply, and true
// if a reply was available, or false if the process exited or closed its reply channel.
func (ctl *Ctl) GetReply() (interface{}, bool) {
	select {
	case _ = <-ctl.Exit:
		return nil, false
	case rep, any := <-ctl.Rep:
		return rep, any
	}
}

// GetRequest receives a request for this process.  It returns the request, and true if a request was
// available, or false if the request channel was closed.  If a cancellation request is pending for this
// process, the process exits instead and GetRequest never returns.
func (env *Env) GetRequest() (interface{}, bool) {
	select {
	case _ = <-env.Cancel:
		env.ExitCanceled()
		return nil, false
	case req, any := <-env.Req:
		return req, any
	}
}

// PutReply sends a reply from this process.  If a cancellation request is pending for this process,
// the process exits instead and PutReply never returns.
func (env *Env) PutReply(rep interface{}) {
	select {
	case _ = <-env.Cancel:
		env.ExitCanceled()
	case env.rep <- rep:
	}
}
