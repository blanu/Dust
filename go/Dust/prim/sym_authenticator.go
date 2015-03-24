package prim

import (
	cryptoSubtle "crypto/subtle"
	"encoding/binary"
	"io"

	"github.com/blanu/Dust/go/Dust/prim/skein"
)

type (
	AuthKey   CValue
	AuthValue CValue
)

type MAC struct {
	*skein.Hash
	args skein.Args
}

var _ io.Writer = (*MAC)(nil)

func (m *MAC) SetKey(key AuthKey) {
	m.args.Key = key[:]
	m.args.Person = skeinPersonalization
}

func (m *MAC) Reset(nonceInt uint64) {
	var nonceArray [8]byte
	binary.BigEndian.PutUint64(nonceArray[:], nonceInt)
	m.args.Nonce = nonceArray[:]
	m.Hash = skein.New(CValueSize, &m.args)
}

type GeneratingMAC struct {
	MAC
}

func (m *GeneratingMAC) Generate() (result AuthValue) {
	if m.args.Nonce == nil {
		panic("Dust/prim: no nonce for MAC")
	}

	_ = m.Sum(result[:0])
	m.args.Nonce = nil
	return
}

func (av AuthValue) Slice() []byte {
	return av[:]
}

type VerifyingMAC struct {
	MAC
	verifyEqual AuthValue
}

func (m *VerifyingMAC) Verify(in []byte) bool {
	if m.args.Nonce == nil {
		panic("Dust/prim: no nonce for MAC")
	}

	_ = m.Sum(m.verifyEqual[:0])
	m.args.Nonce = nil
	return m.verifyEqual.Equal(in)
}

func GenerateMAC(input []byte, nonceInt uint64, key AuthKey) AuthValue {
	var mac GeneratingMAC
	mac.SetKey(key)
	mac.Reset(nonceInt)
	mac.Write(input)
	return mac.Generate()
}

func (av AuthValue) Equal(other []byte) bool {
	// TODO: AuthLen
	return len(other) == 32 && cryptoSubtle.ConstantTimeCompare(av[:], other[:]) == 1
}
