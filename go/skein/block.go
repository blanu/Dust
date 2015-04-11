// Copyright © 2012 Dmitry Chestnykh, 2015 Drake Wilson.  All rights reserved.  Use of this source code is
// governed by a BSD-style license that can be found in the ../LICENSE.md file.

package skein

const (
	blockSizeWords = 8
	blockSizeBytes = blockSizeWords * 8
	blockSizeBits  = blockSizeBytes * 8
)

type Arg uint8

const (
	ArgKey     Arg = 0
	ArgConfig  Arg = 4
	ArgPerson  Arg = 8
	ArgPublic  Arg = 12
	ArgKeyId   Arg = 16
	ArgNonce   Arg = 20
	ArgMessage Arg = 48
	ArgOutput  Arg = 63
)

type Args struct {
	Key       []byte // secret key for MAC, KDF, or stream cipher
	Person    []byte // personalization string
	PublicKey []byte // public key for signature hashing
	KeyId     []byte // key identifier for KDF
	Nonce     []byte // nonce for stream cipher or randomized hashing
}

type tweak [2]uint64

const (
	flagFirst uint64 = 1 << 62
	flagLast  uint64 = 1 << 63
)

func firstTweak(arg Arg) (t tweak) {
	t[0] = 0
	t[1] = uint64(arg)<<56 | flagFirst
	return
}

func blockTweak(n uint64, arg Arg) (t tweak) {
	t[0] = n
	t[1] = uint64(arg)<<56 | flagFirst | flagLast
	return
}

func (t tweak) arg() Arg {
	return Arg((t[1]>>56)&63)
}

type block [blockSizeWords]uint64

func (chain *block) input(t *tweak, in *block) {
	oneStep(chain, chain, t, in)
}

var outputTweak = blockTweak(8, ArgOutput)

func (chain *block) output(n uint64, out *block) {
	var input block
	input[0] = n
	oneStep(out, chain, &outputTweak, &input)
}

func (u *block) consume(data *[]byte) (n int) {
	var b []byte
	if len(*data) >= blockSizeBytes {
		b = (*data)[:blockSizeBytes]
		n = blockSizeBytes
	} else {
		b = make([]byte, 64)
		n = copy(b, *data)
	}

	for i, _ := range u {
		u[i] = uint64(b[i*8+0]) | uint64(b[i*8+1])<<8 | uint64(b[i*8+2])<<16 | uint64(b[i*8+3])<<24 |
			uint64(b[i*8+4])<<32 | uint64(b[i*8+5])<<40 | uint64(b[i*8+6])<<48 | uint64(b[i*8+7])<<56
	}

	*data = (*data)[n:]
	return
}

func (u *block) zero() {
	for i, _ := range u {
		u[i] = 0
	}
}
