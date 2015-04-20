// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

/*
Package huffman implements Huffman streaming encoders and decoders, for 256-symbol tables only,
with up to 256-bit codeword length.
*/
package huffman

type symbol uint8

const totalSymbols = 256

// BitString represents a packed bit string.  Within each octet, bits are addressed most significant first.
//
// Invariants:
//   - 0 <= BitLength <= len(Packed)*8
//   - if BitLength%8 != 0, the low (8 - BitLength%8) bits of Packed[BitLength/8] are zero
type BitString struct {
	Packed    []uint8
	BitLength int
}

// extract extracts req bits starting from offset, where 0 <= req <= 8 and 0 <= offset.  offset may point
// outside the BitString.
func (bs BitString) extract(offset int, req int) (packed uint8, n int) {
	avail := bs.BitLength - offset
	switch {
	case offset < 0:
		panic("huffman: extract from invalid negative offset")
	case req < 0:
		panic("huffman: extract invalid negative number of bits")
	case 8 < req:
		panic("huffman: extract too many bits")
	case req == 0 || avail <= 0:
		return
	case req < avail:
		n = req
	default:
		n = avail
	}

	// Conversion safety: 0 < req above.
	requ := uint(req)
	octetOffset := offset / 8
	// Conversion safety: 0 <= offset above, so 0 <= offset%8 <= 7.
	shift := uint(8 - offset%8)
	packed = bs.Packed[octetOffset] & (1<<shift - 1)
	if requ <= shift {
		packed >>= shift - requ
		return
	} else {
		fromNext := requ - shift
		packed <<= fromNext
		// The next octet is only valid if at least one bit beyond the start of it is valid.
		if (octetOffset+1)*8 < bs.BitLength {
			packed |= (bs.Packed[octetOffset+1] &^ (1<<shift - 1)) >> (8 - fromNext)
		}
		return
	}
}

// check panics if any of the invariants are invalid for bs.
func (bs BitString) check() {
	switch {
	case !(0 <= bs.BitLength):
		panic("huffman: bit string with negative length")
	case !(bs.BitLength <= len(bs.Packed)*8):
		panic("huffman: bit string with insufficient octets to represent it")
	}

	if bs.BitLength%8 != 0 {
		// Conversion safety: 0 < bs.BitLength%8 <= 7.
		shift := uint(8 - bs.BitLength%8)
		lowBits := bs.Packed[bs.BitLength/8] & (uint8(1)<<shift - 1)
		if lowBits != 0 {
			panic("huffman: bit string with extraneous nonzero bits in representation")
		}
	}
}

func (bs BitString) String() string {
	prefix := []rune{'#', '*'}
	allRunes := make([]rune, len(prefix)+bs.BitLength)
	copy(allRunes, prefix)

	bitRunes := allRunes[len(prefix):]
	for i, _ := range bitRunes {
		bit := (bs.Packed[i/8] >> uint(7-i%8)) & 1
		if bit == 0 {
			bitRunes[i] = '0'
		} else {
			bitRunes[i] = '1'
		}
	}

	return string(allRunes)
}
