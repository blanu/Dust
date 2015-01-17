package DustCrypto

import (
	"crypto/cipher"

	"github.com/dchest/skein"
)

// NewStreamCipher takes a key and an IV and returns a stateful keystream.
func NewStreamCipher(key SecretBytes, iv [32]byte) cipher.Stream {
	return skein.NewStream(key.Slice(), iv[:])
}
