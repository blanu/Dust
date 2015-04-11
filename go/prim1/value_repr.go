// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package prim

import (
	"encoding/base32"
	"errors"
	"strings"
)

var (
	ErrInvalidCValue = errors.New("Dust/prim: not a CValue representation")
)

func decodeText(out *[32]byte, in string, actualError error) error {
	if len(in) != 52 {
		return actualError
	}
	b, err := base32.StdEncoding.DecodeString(in + `====`)
	if err != nil {
		return actualError
	}
	copy(out[:], b)
	return nil
}

func encodeText(in *[32]byte) (result string) {
	result = strings.TrimRight(base32.StdEncoding.EncodeToString(in[:]), `=`)
	if len(result) != 52 {
		panic("Dust/prim: weird Base32 consistency error")
	}
	return
}

func (cval CValue) Text() string {
	array := [32]byte(cval)
	return encodeText(&array)
}

func CValueFromText(text string) (out CValue, err error) {
	var array [32]byte
	err = decodeText(&array, text, ErrInvalidCValue)
	out = CValue(array)
	return
}
