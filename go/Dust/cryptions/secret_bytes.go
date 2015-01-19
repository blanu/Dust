package cryptions

import (
	cryptoSubtle "crypto/subtle"
	cryptoRand "crypto/rand"
)

var zeros [32]byte

// SecretBytes uses an extra pointer indirection so the contents are less likely to be copied and so some
// limited amount of private-key memory wiping might be implementable.  (Go's memory model doesn't really
// guarantee that this is possible to begin with, though!  So don't rely on that as HardSecurity for your
// secrets.  Maybe there's a better way to do this with funky allocator tricks, but even then the registers
// and stack will probably leak things here and there.  Sigh.)
type SecretBytes interface {
	Pointer() *[32]byte
	Slice() []byte
	Destroy()
}

type secretBytes struct {
	pointer *[32]byte
}

func (sb *secretBytes) Pointer() *[32]byte {
	if sb.pointer == nil {
		panic("invalid access to destroyed SecretBytes")
	}
	return sb.pointer
}

func (sb *secretBytes) Slice() []byte {
	if sb.pointer == nil {
		panic("invalid access to destroyed SecretBytes")
	}
	return sb.pointer[:]
}

func (sb *secretBytes) Destroy() {
	if sb.pointer == nil {
		return
	}
	// Voodoo in the hopes that this will do the equivalent of explicit_bzero.
	cryptoSubtle.ConstantTimeCopy(1, sb.Slice(), zeros[:])
	sb.pointer = nil
}

func NewSecretBytes() SecretBytes {
	return &secretBytes{&[32]byte{}}
}

func RandomizeSecretBytes(sb SecretBytes) error {
	_, err := cryptoRand.Read(sb.Slice())
	return err
}
