package prim

import (
	"encoding/base32"
	"strings"
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
