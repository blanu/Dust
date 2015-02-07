package prim

import (
	cryptoRand "crypto/rand"
)

const CValueSize = 32

type CValue [CValueSize]byte

var zeroCValue CValue

var skeinPersonalization = []byte(`tag:blanu.net,2015:Dust2015/skein-personalization`)

func ZeroCValue() CValue {
	return zeroCValue
}

func RandomCValue() CValue {
	var result [CValueSize]byte
	_, err := cryptoRand.Read(result[:])
	if err != nil {
		panic("Dust/prim: somehow out of entropy")
	}

	return result
}
