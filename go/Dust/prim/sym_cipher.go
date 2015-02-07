package prim

import (
	"crypto/cipher"

	"github.com/dchest/skein"
)

type (
	CipherKey CValue
)

type Cipher struct {
	cipher.Stream
}

var _ cipher.Stream = (*Cipher)(nil)

var cipherNonce = []byte(`tag:blanu.net,2015:Dust2015/skein-cipher`)

func (c *Cipher) SetKey(key CipherKey) {
	c.Stream = skein.NewStream(key[:], cipherNonce)
}

func (c *Cipher) SetRandomKey() {
	c.SetKey(CipherKey(RandomCValue()))
}
