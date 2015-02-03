package cryptions

import (
	"github.com/dchest/skein"
)

// TODO: use type system to separate shared secrets from actual keys.

// DeriveKey overwrites keyOut with a key deterministically derived from the secret using the name.
func DeriveKey(secret SecretBytes, name string, keyOut SecretBytes) {
	// Currently, Skein-KDF.
	args := skein.Args{
		Key:   secret.Slice(),
		KeyId: []byte(name),
	}

	hash := skein.New(32, &args)
	copy(keyOut.Slice(), hash.Sum(keyOut.Slice()[:0]))
}
