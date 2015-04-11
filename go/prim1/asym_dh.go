// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package prim

import (
	"errors"

	"code.google.com/p/go.crypto/curve25519"
	"github.com/agl/ed25519/extra25519"
)

var (
	ErrBadPrivateKey = errors.New("Dust/prim: bad private key")
	ErrBadPublicKey  = errors.New("Dust/prim: bad public key")
)

type Public struct {
	public  [32]byte
	uniform [32]byte
}

func (public *Public) recompute() (err error) {
	extra25519.RepresentativeToPublicKey(&public.public, &public.uniform)
	return
}

type Private struct {
	Public
	private CValue
}

func (private *Private) recompute() (err error) {
	acceptable := extra25519.ScalarBaseMult(&private.public, &private.uniform, (*[32]byte)(&private.private))
	if !acceptable {
		err = ErrBadPrivateKey
	}
	return
}

func maskPrivate(c *CValue) {
	c[0] &= 248
	c[31] &= 127
	c[31] |= 64
}

func NewPrivate() (result Private) {
	acceptable := false
	for tries := 0; !acceptable && tries < 256; {
		result.private = RandomCValue()
		maskPrivate(&result.private)
		acceptable = result.recompute() == nil
	}

	if !acceptable {
		panic("Dust/prim: too many failures generating private key")
	}
	return
}

func (private Private) SharedSecret(public Public) Secret {
	var raw CValue
	curve25519.ScalarMult((*[32]byte)(&raw), (*[32]byte)(&private.private), &public.public)
	return Secret(raw)
}
