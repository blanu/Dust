// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package crypting

import (
	"errors"
	"net"
)

var (
	ErrBadHandshake     = errors.New("Dust/crypting: bad handshake")
	ErrBadDecode        = errors.New("Dust/crypting: bad decode")
	ErrNotSupported     = errors.New("Dust/crypting: operation not supported")
)

type errTimeout struct{}
var ErrTimeout net.Error = (*errTimeout)(nil)

func (_ *errTimeout) Error() string {
	return "Dust/crypting: I/O timeout"
}

func (_ *errTimeout) Timeout() bool {
	return true
}

func (_ *errTimeout) Temporary() bool {
	return false
}

type errInterrupted struct{}
var ErrInterrupted net.Error = (*errInterrupted)(nil)

func (_ *errInterrupted) Error() string {
	return "Dust/crypting: I/O interrupted"
}

func (_ *errInterrupted) Timeout() bool {
	return false
}

func (_ *errInterrupted) Temporary() bool {
	return true
}

type errCrashInterrupted struct{}
var ErrCrashInterrupted net.Error = (*errCrashInterrupted)(nil)

func (_ *errCrashInterrupted) Error() string {
	return "Dust/crypting: I/O interrupted (irreversibly)"
}

func (_ *errCrashInterrupted) Timeout() bool {
	return false
}

func (_ *errCrashInterrupted) Temporary() bool {
	return true
}
