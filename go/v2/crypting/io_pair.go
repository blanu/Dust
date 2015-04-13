// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package crypting

import (
	"io"
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

//func (pair *ioPair) CloseRead() error {
//	return pair.rd.CloseRead()
//}

func (pair *ioPair) Write(p []byte) (n int, err error) {
	return pair.wr.Write(p)
}

//func (pair *ioPair) CloseWrite() error {
//	return pair.wr.CloseWrite()
//}
