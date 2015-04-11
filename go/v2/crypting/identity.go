// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

/*
Package crypting implements the cryptographic and framing layer of the Dust protocol suite.
*/
package crypting

import (
	"errors"

	"github.com/blanu/Dust/go/prim1"
)

var (
	ErrBadOpaqueId = errors.New("Dust/crypting: bad opaque ID")
)

type OpaqueId prim.CValue

// Public holds the longterm public key and shared identifier for a server.
type Public struct {
	Id  OpaqueId
	Key prim.Public
}

// Private holds the longterm private key and shared identifier for a server.
type Private struct {
	Id  OpaqueId
	Key prim.Private
}

func NewOpaqueId() *OpaqueId {
	opaque := OpaqueId(prim.RandomCValue())
	return &opaque
}

func (opaque OpaqueId) Text() string {
	return prim.CValue(opaque).Text()
}

func LoadOpaqueIdText(text string) (*OpaqueId, error) {
	if cvalue, err := prim.CValueFromText(text); err == nil {
		opaque := OpaqueId(cvalue)
		return &opaque, nil
	} else {
		return nil, ErrBadOpaqueId
	}
}
