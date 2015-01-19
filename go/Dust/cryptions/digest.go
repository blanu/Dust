package cryptions

import (
	"github.com/dchest/skein"
)

type Digest struct {
	*skein.Hash
}

func NewDigest() Digest {
	return Digest{skein.New(32, nil)}
}

func (d Digest) WriteSecret(sb SecretBytes) {
	d.Hash.Write(sb.Slice())
}

func (d Digest) SumSecret(sbOut SecretBytes) {
	n, err := d.Hash.OutputReader().Read(sbOut.Slice())
	if n != 32 || err != nil {
		panic("pure Skein should never fail a read")
	}
}

func (d Digest) Destroy() {
	// TODO: Doesn't do anything right now, because the Skein doesn't provide this
	// (but it's still unclear whether or not to just give up on this).
}
