package cryptions

import (
	cryptoSubtle "crypto/subtle"
	"hash"

	"github.com/dchest/skein"
)

type MAC interface {
	hash.Hash
	SumSecret(sbOut SecretBytes)
	Verify(expected []byte) bool
}

type skeinMAC struct {
	*skein.Hash
	outBuf [32]byte
}

func (sm *skeinMAC) Verify(expected []byte) bool {
	if len(expected) != 32 {
		return false
	}

	actual := sm.Sum(sm.outBuf[:0])
	return cryptoSubtle.ConstantTimeCompare(actual, expected) == 1
}

func (sm *skeinMAC) SumSecret(sbOut SecretBytes) {
	n, err := sm.Hash.OutputReader().Read(sbOut.Slice())
	if n != 32 || err != nil {
		panic("Dust/cryptions: pure Skein should never fail a read")
	}
}

func NewMAC(key SecretBytes) MAC {
	args := skein.Args{
		Key: key.Slice(),
	}
	sk := skein.New(32, &args)

	return &skeinMAC{Hash: sk}
}
