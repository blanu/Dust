package DustCrypto

import (
	"crypto/cipher"
)

// INCOMPLETE (entire file): where's the Skein?  Do something silly instead.

type StreamCipher struct {
	bogus [32]byte
	i uint8
}

// NewStreamCipher takes a key and an IV and returns a stateful keystream.
func NewStreamCipher(key SecretBytes, iv [32]byte) cipher.Stream {
	sc := &StreamCipher{}
	keyPtr := key.Pointer()
	
	for i := 0; i < 32; i++ {
		sc.bogus[i] = keyPtr[i] ^ iv[i]
	}

	return sc
}

func (sc *StreamCipher) XORKeyStream(dst []byte, src []byte) {
	for i, n := 0, len(src); i < n; i++ {
		dst[i] = src[i] ^ sc.bogus[sc.i]
		sc.i = (sc.i + 1) % 32
	}
}
