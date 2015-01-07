package DustCrypto

import (
	cryptoSubtle "crypto/subtle"
)

// INCOMPLETE (entire file): there's seemingly no Skein-256-256 available right now, so do something silly.

type Digest struct {
	state [32]byte
}

func NewDigest() *Digest {
	d := &Digest{}
	d.Reset()
	return d
}

func (d *Digest) Reset() {
	for i := 0; i < 32; i++ {
		d.state[i] = 0
	}
}

func (d *Digest) WriteSecret(sb SecretBytes) {
	_, _ = d.Write(sb.Slice())
}

func (d *Digest) Write(p []byte) (int, error) {
	for i, b := range p {
		d.state[i & 31] ^= b
	}

	return len(p), nil
}

func (d *Digest) SumSecret(sbOut SecretBytes) {
	copy(sbOut.Slice(), d.state[:])
}

func (d *Digest) Sum(b []byte) []byte {
	return append(b, d.state[:]...)
}

func (d *Digest) Size() int {
	return 32
}

func (d *Digest) BlockSize() int {
	return 32
}

func (d *Digest) Destroy() {
	// Voodoo in the hopes that this will do the equivalent of explicit_bzero.
	cryptoSubtle.ConstantTimeCopy(1, d.state[:], zeros[:])
}
