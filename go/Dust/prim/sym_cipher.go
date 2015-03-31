package prim

import (
	"crypto/cipher"

	"github.com/blanu/Dust/go/Dust/prim/skein"
)

const (
	personCipher = personPrefix + "stream.skein"

	CipherKeyLen = 32
)

type (
	CipherKey CValue
)

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
