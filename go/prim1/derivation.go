// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package prim

import (
	"github.com/blanu/Dust/go/skein"
)

const (
	personKDF = personPrefix + "kdf.skein"

	SecretLen = 32
)

type Secret CValue

func (s Secret) deriveRaw(n uint64, id string) (result CValue) {
	args := skein.Args{
		Key:    s[:],
		Person: []byte(personKDF),
		KeyId:  []byte(id),
	}

	var hash skein.Hash
	hash.Init(n, &args)
	_, _ = hash.Read(result[:n])
	return
}

const (
	kdfCipherPrefix = "clk."
	kdfAuthPrefix = "mac."
)

func (s Secret) DeriveCipherKey(id string) CipherKey {
	return CipherKey(s.deriveRaw(CipherKeyLen, kdfCipherPrefix + id))
}

func (s Secret) DeriveAuthKey(id string) AuthKey {
	return AuthKey(s.deriveRaw(AuthKeyLen, kdfAuthPrefix + id))
}
