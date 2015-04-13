package model1

import (
	"time"

	"github.com/blanu/Dust/go/huffman"
)

type Encoder struct {
	SleepDist  Rand
	LengthDist Rand
	Prefix     []byte
	Unhuffer   *huffman.Decoder

	position uint64
}

func (half *HalfModel) NewEncoderIsh() *Encoder {
	return &Encoder{
		Prefix:   half.Prefix,
		Unhuffer: huffman.NewDecoder(half.HuffCoding),
		position: 0,
	}
}

func (enc *Encoder) MaxPacketLength() uint16 {
	return 65535
}

func (enc *Encoder) NextPacketLength() uint16 {
	return clampUint16(enc.LengthDist.Rand())
}

func (enc *Encoder) NextPacketSleep() time.Duration {
	// TODO: really clampUint16 here?
	return time.Duration(clampUint16(enc.LengthDist.Rand())) * time.Millisecond
}

func (enc *Encoder) ShapeBytes(dst, src []byte) (dn, sn int) {
	if enc.position < uint64(len(enc.Prefix)) {
		fixedn := copy(dst, enc.Prefix[enc.position:])
		dst = dst[fixedn:]
		dn += fixedn
		enc.position += uint64(fixedn)
	}

	hdn, hsn := enc.Unhuffer.Decode(dst, src)
	enc.position += uint64(hdn)
	dn += hdn
	sn += hsn
	return
}
