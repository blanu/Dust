package prim

import (
	cryptoRand "crypto/rand"
)

const (
	CValueLen = 32
)

type CValue [CValueLen]byte

var zeroCValue CValue

const personPrefix = "tag:blanu.net,2015:Dust2015/"

func ZeroCValue() CValue {
	return zeroCValue
}

func RandomCValue() CValue {
	var result [CValueLen]byte
	_, err := cryptoRand.Read(result[:])
	if err != nil {
		panic("Dust/prim: somehow out of entropy")
	}

	return result
}
