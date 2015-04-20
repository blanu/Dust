// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package crypting

import (
	"io"
	"time"
)

type pairReader interface {
	io.Reader
}

type pairWriter interface {
	io.Writer
}

type ioPair struct {
	rd pairReader
	wr pairWriter
}

func (pair *ioPair) Read(p []byte) (n int, err error) {
	return pair.rd.Read(p)
}

type canCloseRead interface {
	CloseRead() error
}

func (pair *ioPair) CloseRead() error {
	rd_, ok := pair.rd.(canCloseRead)
	if ok {
		return rd_.CloseRead()
	} else {
		return ErrNotSupported
	}
}

type canSetReadDeadline interface {
	SetReadDeadline(t time.Time) error
}

func (pair *ioPair) SetReadDeadline(t time.Time) error {
	rd_, ok := pair.rd.(canSetReadDeadline)
	if ok {
		return rd_.SetReadDeadline(t)
	} else {
		return ErrNotSupported
	}
}

type canSetReadInterrupt interface {
	SetReadInterrupt(ch <-chan struct{}) error
}

func (pair *ioPair) SetReadInterrupt(ch <-chan struct{}) error {
	rd_, ok := pair.rd.(canSetReadInterrupt)
	if ok {
		return rd_.SetReadInterrupt(ch)
	} else {
		return ErrNotSupported
	}
}

func (pair *ioPair) Write(p []byte) (n int, err error) {
	return pair.wr.Write(p)
}

type canCloseWrite interface {
	CloseWrite() error
}

func (pair *ioPair) CloseWrite() error {
	wr_, ok := pair.wr.(canCloseWrite)
	if ok {
		return wr_.CloseWrite()
	} else {
		return ErrNotSupported
	}
}

type canSetWriteDeadline interface {
	SetWriteDeadline(t time.Time) error
}

func (pair *ioPair) SetWriteDeadline(t time.Time) error {
	wr_, ok := pair.wr.(canSetWriteDeadline)
	if ok {
		return wr_.SetWriteDeadline(t)
	} else {
		return ErrNotSupported
	}
}

type canSetWriteInterrupt interface {
	SetWriteInterrupt(ch <-chan struct{}) error
}

func (pair *ioPair) SetWriteInterrupt(ch <-chan struct{}) error {
	wr_, ok := pair.wr.(canSetWriteInterrupt)
	if ok {
		return wr_.SetWriteInterrupt(ch)
	} else {
		return ErrNotSupported
	}
}

func (pair *ioPair) SetDeadline(t time.Time) error {
	rd_, rok := pair.rd.(canSetReadDeadline)
	wr_, wok := pair.wr.(canSetWriteDeadline)
	if !(rok && wok) {
		return ErrNotSupported
	}
	
	rerr := rd_.SetReadDeadline(t)
	werr := wr_.SetWriteDeadline(t)
	switch {
	case rerr != nil:
		return rerr
	case werr != nil:
		return werr
	default:
		return nil
	}
}

func (pair *ioPair) SetInterrupt(ch <-chan struct{}) error {
	rd_, rok := pair.rd.(canSetReadInterrupt)
	wr_, wok := pair.wr.(canSetWriteInterrupt)
	if !(rok && wok) {
		return ErrNotSupported
	}
	
	rerr := rd_.SetReadInterrupt(ch)
	werr := wr_.SetWriteInterrupt(ch)
	switch {
	case rerr != nil:
		return rerr
	case werr != nil:
		return werr
	default:
		return nil
	}
}
