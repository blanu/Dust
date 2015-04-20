// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package prim

const (
	PublicBinaryLen  = 32
	PrivateBinaryLen = 32
	PublicTextLen    = 52
	PrivateTextLen   = 52
)

func LoadPublicBinary(b []byte) (result Public, err error) {
	if len(b) != 32 {
		return Public{}, ErrBadPublicKey
	}
	copy(result.uniform[:], b)
	err = result.recompute()
	return
}

func LoadPublicText(s string) (result Public, err error) {
	err = decodeText((*[32]byte)(&result.uniform), s, ErrBadPublicKey)
	if err != nil {
		return
	}
	err = result.recompute()
	return
}

func LoadPrivateBinary(b []byte) (result Private, err error) {
	if len(b) != 32 {
		return Private{}, ErrBadPrivateKey
	}
	copy(result.private[:], b)
	err = result.recompute()
	return
}

func LoadPrivateText(s string) (result Private, err error) {
	err = decodeText((*[32]byte)(&result.private), s, ErrBadPrivateKey)
	if err != nil {
		return
	}
	err = result.recompute()
	return
}

func (public Public) Binary() []byte {
	return public.uniform[:]
}

func (public Public) Text() string {
	return encodeText((*[32]byte)(&public.uniform))
}

func (private Private) PrivateBinary() []byte {
	return private.private[:]
}

func (private Private) PrivateText() string {
	return encodeText((*[32]byte)(&private.private))
}
