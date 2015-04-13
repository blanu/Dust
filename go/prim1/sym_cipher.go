// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package prim

import (
	"crypto/cipher"

	"github.com/blanu/Dust/go/skein"
)

const (
	personCipher = personPrefix + "stream.skein"

	CipherKeyLen = 32
)

type (
	CipherKey CValue
)

func ZeroCipherKey() CipherKey {
	return CipherKey(zeroCValue)
}

type Cipher struct {
	skein.Hash
}

var _ cipher.Stream = (*Cipher)(nil)

func (c *Cipher) SetKey(key CipherKey) {
	args := skein.Args{
		Key:    key[:],
		Person: []byte(personCipher),
		Nonce:  nil,
	}

	c.Hash.Init(^uint64(0), &args)
	c.Hash.CloseWrite()
}

func (c *Cipher) SetRandomKey() {
	c.SetKey(CipherKey(RandomCValue()))
}

func (c *Cipher) XORKeyStream(dst, src []byte) {
	c.Hash.XORKeyStream(dst, src)
}
