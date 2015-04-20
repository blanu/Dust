// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package prim

import (
	cryptoSubtle "crypto/subtle"
	"io"

	"github.com/blanu/Dust/go/skein"
)

const (
	personMAC = personPrefix + "mac.skein"

	AuthKeyLen = 32
	AuthLen = 32
)

type (
	AuthKey   CValue
	AuthValue CValue
)

func ZeroAuthKey() AuthKey {
	return AuthKey(zeroCValue)
}

func ZeroAuthValue() AuthValue {
	return AuthValue(zeroCValue)
}

type MAC struct {
	skein.Hash

	keyInitial skein.Initial
	nonceSet   bool
}

var _ io.Writer = (*MAC)(nil)

func (m *MAC) SetKey(key AuthKey) {
	m.keyInitial.Init(AuthLen, &skein.Args{Key: key[:], Person: []byte(personMAC)})
	m.nonceSet = false
}

func (m *MAC) Reset(nonceInt uint64) {
	var nonceBytes [8]byte
	nonceBytes[0] = byte(nonceInt >> 56)
	nonceBytes[1] = byte(nonceInt >> 48)
	nonceBytes[2] = byte(nonceInt >> 40)
	nonceBytes[3] = byte(nonceInt >> 32)
	nonceBytes[4] = byte(nonceInt >> 24)
	nonceBytes[5] = byte(nonceInt >> 16)
	nonceBytes[6] = byte(nonceInt >> 8)
	nonceBytes[7] = byte(nonceInt)

	afterNonce := m.keyInitial
	afterNonce.AddArg(skein.ArgNonce, nonceBytes[:])
	afterNonce.Finish()
	m.Hash.InitFrom(&afterNonce)
	m.nonceSet = true
}

type GeneratingMAC struct {
	MAC
}

func (m *GeneratingMAC) Generate() (result AuthValue) {
	if !m.nonceSet {
		panic("Dust/prim: no nonce for MAC")
	}

	_, _ = m.Read(result[:])
	m.nonceSet = false
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
	if !m.nonceSet {
		panic("Dust/prim: no nonce for MAC")
	}

	_, _ = m.Read(m.verifyEqual[:])
	m.nonceSet = false
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
	return len(other) == AuthLen && cryptoSubtle.ConstantTimeCompare(av[:], other[:]) == 1
}
