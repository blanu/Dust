package prim

import (
	"github.com/blanu/Dust/go/Dust/prim/skein"
)

type (
	Secret CValue
)

type SecretDigest struct {
	*skein.Hash
}

func (d *SecretDigest) Init() {
	args := skein.Args{
		Person: skeinPersonalization,
	}
	d.Hash = skein.New(CValueSize, &args)
}

func (d *SecretDigest) WriteSecret(secret Secret) {
	d.Write(secret[:])
}

func (d *SecretDigest) Finish() (result Secret) {
	_ = d.Sum(result[:0])
	return
}

func (s Secret) deriveRaw(id string) CValue {
	args := skein.Args{
		Key:   s[:],
		KeyId: []byte(id),
	}

	var result CValue
	skein.New(CValueSize, &args).Sum(result[:0])
	return result
}

func (s Secret) DeriveCipherKey(id string) CipherKey {
	return CipherKey(s.deriveRaw(`clk.` + id))
}

func (s Secret) DeriveAuthKey(id string) AuthKey {
	return AuthKey(s.deriveRaw(`mac.` + id))
}
