// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

/*
Package sillyHex implements the contrived, silly "sillyHex" model for Dust.  This model should not be used
for serious applications!  Importing this package registers its model.
*/
package sillyHex

import (
	"math/rand"
	"time"

	"github.com/blanu/Dust/go/v2/interface"
)

type sillyHexCodec struct {
	inTopNybble, outBotNybble                int8
	lenNext, lenMin, lenStep, lenMax         uint16
	sleepNext, sleepMin, sleepStep, sleepMax time.Duration
}

var (
	hexAlphabet = []byte("0123456789abcdef")
)

type sillyHexModel struct {
}

func newSillyHexCodec() Dust.ShapingCodec {
	return &sillyHexCodec{
		inTopNybble:  -1,
		outBotNybble: -1,

		lenNext:   800,
		lenMin:    800,
		lenStep:   50,
		lenMax:    1000,
		sleepNext: 20 * time.Millisecond,
		sleepMin:  20 * time.Millisecond,
		sleepStep: 5 * time.Millisecond,
		sleepMax:  50 * time.Millisecond,
	}
}

func (model *sillyHexModel) MakeClientPair() (Dust.ShapingEncoder, Dust.ShapingDecoder, error) {
	codec := newSillyHexCodec()
	return codec, codec, nil
}

func (model *sillyHexModel) MakeServerPair() (Dust.ShapingEncoder, Dust.ShapingDecoder, error) {
	codec := newSillyHexCodec()
	return codec, codec, nil
}

func makeSillyHexModel(params map[string]string) (Dust.ShapingModel, error) {
	if err := Dust.CheckUnackedParams(params, nil); err != nil {
		return nil, err
	}

	return &sillyHexModel{}, nil
}

func init() {
	Dust.RegisterModel("sillyHex", makeSillyHexModel)
}

func (hex *sillyHexCodec) MaxPacketLength() uint16 {
	return hex.lenMax
}

func (hex *sillyHexCodec) NextPacketLength() uint16 {
	hex.lenNext += hex.lenStep
	if hex.lenNext > hex.lenMax {
		hex.lenNext = hex.lenMin
	}

	return hex.lenNext
}

func (hex *sillyHexCodec) NextPacketSleep() time.Duration {
	hex.sleepNext += hex.sleepStep
	if hex.sleepNext > hex.sleepMax {
		hex.sleepNext = hex.sleepMin
	}

	return hex.sleepNext
}

func (hex *sillyHexCodec) ShapeBytes(dst, src []byte) (dn, sn int) {
	pattern := uint64(rand.Int63())

	maybeIntersperse := func() {
		if pattern&1 != 0 && dn < len(dst) {
			dst[dn] = '.'
			dn++
		}
		pattern = pattern>>1 | pattern<<63
	}

	for dn < len(dst) {
		if hex.outBotNybble >= 0 {
			dst[dn] = hexAlphabet[hex.outBotNybble]
			dn++
			hex.outBotNybble = -1
			maybeIntersperse()
		} else if sn == len(src) {
			return
		} else {
			byte := src[sn]
			sn++
			dst[dn] = hexAlphabet[byte>>4]
			dn++
			hex.outBotNybble = int8(byte & 0xf)
			maybeIntersperse()
		}
	}

	return
}

func (hex *sillyHexCodec) UnshapeBytes(dst, src []byte) (dn, sn int) {
	for sn < len(src) {
		byte := src[sn]
		sn++

		var nybble int8 = -1
		if '0' <= byte && byte <= '9' {
			nybble = int8(byte - '0')
		} else if 'a' <= byte && byte <= 'f' {
			nybble = int8(10 + (byte - 'a'))
		} else if 'A' <= byte && byte <= 'F' {
			nybble = int8(10 + (byte - 'A'))
		} else {
			continue
		}

		if hex.inTopNybble < 0 {
			hex.inTopNybble = nybble
		} else if dn == len(dst) {
			// Unread last nybble-containing byte.
			sn--
			return
		} else {
			dst[dn] = uint8(hex.inTopNybble)<<4 | uint8(nybble)
			dn++
			hex.inTopNybble = -1
		}
	}

	return
}
