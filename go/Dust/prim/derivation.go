package prim

import (
	"github.com/blanu/Dust/go/Dust/prim/skein"
)

const (
	personSecret = personPrefix + "secret.skein"
	personKDF = personPrefix + "kdf.skein"

	SecretLen = 32
)

type Secret CValue

type SecretDigest struct {
	skein.Hash
}

func makeInitial(osize uint64, args *skein.Args) (out skein.Initial) {
	out.Init(osize, args)
	return
}

var secretInitial = makeInitial(CValueLen, &skein.Args{Person: []byte(personSecret)})

func (d *SecretDigest) Init() {
	d.Hash.InitFrom(&secretInitial)
}

func (d *SecretDigest) WriteSecret(secret Secret) {
	d.Write(secret[:])
}

func (d *SecretDigest) Finish() (result Secret) {
	_, _ = d.Read(result[:])
	return
}

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
