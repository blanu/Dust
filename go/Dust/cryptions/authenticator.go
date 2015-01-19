package cryptions

import (
	"crypto/hmac"
	"hash"
)

func ComputeAuthenticator(message []byte, key SecretBytes) []byte {
	mac := hmac.New(func() hash.Hash { return NewDigest() }, key.Slice())
	mac.Write(message)
	return mac.Sum(nil)
}

func VerifyAuthenticator(message []byte, key SecretBytes, receivedAuthenticator []byte) bool {
	// This should always be true anyway.
	if len(receivedAuthenticator) != 32 {
		return false
	}

	mac := hmac.New(func() hash.Hash { return NewDigest() }, key.Slice())
	mac.Write(message)
	expected := mac.Sum(nil)
	return hmac.Equal(receivedAuthenticator, expected)
}
