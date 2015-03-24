package prim

import (
	"crypto/cipher"

	"github.com/blanu/Dust/go/Dust/prim/skein"
)

type (
	CipherKey CValue
)

type Cipher struct {
	cipher.Stream
}

var _ cipher.Stream = (*Cipher)(nil)

var cipherNonce = []byte(``)

func (c *Cipher) SetKey(key CipherKey) {
	c.Stream = skein.NewStream(key[:], cipherNonce)
}

func (c *Cipher) SetRandomKey() {
	c.SetKey(CipherKey(RandomCValue()))
}
