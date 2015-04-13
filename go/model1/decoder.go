package model1

import (
	"github.com/blanu/Dust/go/huffman"
)

type Decoder struct {
	Prefix   []byte
	Enhuffer *huffman.Encoder

	position uint64
}

func (half *HalfModel) NewDecoderIsh() *Decoder {
	return &Decoder{
		Prefix:   half.Prefix,
		Enhuffer: huffman.NewEncoder(half.HuffCoding),
	}
}

func (dec *Decoder) UnshapeBytes(dst, src []byte) (dn, sn int) {
	if dec.position < uint64(len(dec.Prefix)) {
		skip := int(uint64(len(dec.Prefix)) - dec.position)
		if len(src) < skip {
			skip = len(src)
		}

		// NOTE: this doesn't do any comparison with the expected content.
		src = src[skip:]
		sn += skip
		dec.position += uint64(skip)
	}

	hdn, hsn := dec.Enhuffer.Encode(dst, src)
	dec.position += uint64(hsn)
	dn += hdn
	sn += hsn
	return
}
