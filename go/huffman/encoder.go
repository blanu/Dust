// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package huffman

type bitWriter struct {
	dst      []byte
	di       int
	lowAvail int
}

func newBitWriter(dst []byte) bitWriter {
	if len(dst) > 0 {
		dst[0] = byte(0)
	}
	return bitWriter{dst, 0, 8}
}

// writeBits writes bits from src into bwr, starting at the given bit offset.  It returns the number of bits
// successfully written.
func (bwr *bitWriter) writeBits(src BitString, offset int) (wroteBits int) {
	si := offset
	for bwr.di < len(bwr.dst) {
		packed, n := src.extract(si, 8)
		if n == 0 {
			break
		}
		si += 8

		// Conversion safety: 0 < bwr.lowAvail <= 8
		// Shift position 8 down to position bwr.lowAvail.
		bwr.dst[bwr.di] |= packed >> uint(8-bwr.lowAvail)
		bwr.lowAvail -= n

		if bwr.lowAvail > 0 {
			// We must have had fewer than 8 bits to write.  Since that can only happen if we hit
			// the end of src, there's no more to write.
			wroteBits += n
			break
		} else if bwr.di+1 == len(bwr.dst) {
			// Don't count the unwritable -bwr.lowAvail bits.  (This has to come first because
			// we reset bwr.lowAvail below.
			wroteBits += n - (-bwr.lowAvail)

			// Align to end of destination buffer.  We can't write any more.
			bwr.di++
			bwr.lowAvail = 8
			break
		} else {
			// Move to next octet to write remaining bits.
			bwr.di++
			bwr.lowAvail += 8

			// Conversion safety: bwr.lowAvail = (orig + (8 - n)); 0 < orig <= 8.
			// Shift position 8 - n up to position bwr.lowAvail.
			bwr.dst[bwr.di] = packed << uint(bwr.lowAvail-(8-n))
			wroteBits += n
		}
	}

	return
}

// backOut backs out the output to the most recent complete octet and returns the parts backed over.
func (bwr *bitWriter) backOut() (octet uint8, bits int) {
	if bwr.lowAvail == 8 {
		return
	} else {
		octet = bwr.dst[bwr.di]
		bits = 8 - bwr.lowAvail
		bwr.lowAvail = 8
		bwr.dst[bwr.di] = 0
		return
	}
}

// padOut pads the output to the next complete octet.  It always succeeds, because the destination must have
// already had enough space for a full octet if a partial octet has been written.
func (bwr *bitWriter) padOut() {
	if bwr.lowAvail == 8 {
		return
	} else {
		bwr.lowAvail = 8
		bwr.di++
	}
}

func (bwr bitWriter) aligned() bool {
	return bwr.lowAvail == 8
}

// wroteOctets returns the number of complete octets written so far.
func (bwr bitWriter) wroteOctets() int {
	return bwr.di
}

type bitStringTail struct {
	BitString
	tailOffset int
}

func (tail *bitStringTail) empty() bool {
	return tail.tailOffset == tail.BitLength
}

func (tail *bitStringTail) remaining() int {
	return tail.BitLength - tail.tailOffset
}

func (tail *bitStringTail) drain(bwr *bitWriter) {
	if tail.tailOffset < tail.BitLength {
		tail.tailOffset += bwr.writeBits(tail.BitString, tail.tailOffset)
	}
}

// Encoder holds state for a single streaming Huffman encoding, converting one byte stream into another.
type Encoder struct {
	coding *Coding

	// backoutTail comes before codeTail.
	backoutTail, codeTail bitStringTail

	backoutStorage [1]byte
}

// NewEncoder constructs a stateful encoder for the given coding.
func NewEncoder(coding *Coding) (enc *Encoder) {
	enc = &Encoder{
		coding: coding,
	}

	enc.backoutTail.Packed = enc.backoutStorage[:]
	return
}

// Encode continues encoding bytes from src into dst, stopping when either no further source bytes can be
// consumed or no further destination bytes can be written.  It returns the number of bytes written to dst and
// consumed from src, respectively.
func (enc *Encoder) Encode(dst []byte, src []byte) (dn int, sn int) {
	codeTable := enc.coding.codeTable
	bwr := newBitWriter(dst)
	enc.backoutTail.drain(&bwr)
	enc.codeTail.drain(&bwr)
	if !enc.backoutTail.empty() || !enc.codeTail.empty() || len(src) == 0 {
		return bwr.wroteOctets(), 0
	}

	si := 0
	for si < len(src) {
		code := codeTable[src[si]]
		si++

		wroteBits := bwr.writeBits(code, 0)
		if wroteBits < code.BitLength {
			enc.codeTail = bitStringTail{code, wroteBits}
			break
		}
	}

	enc.backoutTail.Packed[0], enc.backoutTail.BitLength = bwr.backOut()
	enc.backoutTail.tailOffset = 0
	return bwr.wroteOctets(), si
}

// Aligned returns true iff the input consumed so far corresponds exactly to the output produced so far or
// a successful Flush has been performed.
func (enc *Encoder) Aligned() bool {
	return enc.backoutTail.empty() && enc.codeTail.empty()
}

// Flush pads the output with zero bits to the next byte and attempts to write any pending output into dst.
// It returns the number of bytes written into dst and whether or not all pending output has been successfully
// written.  Since this padding is not delimited, the resultant output stream ceases to be a consistent Huffman
// coded stream at this point; if resynchronization is necessary it must be done elsewhere.
func (enc *Encoder) Flush(dst []byte) (dn int, finished bool) {
	bwr := newBitWriter(dst)
	enc.backoutTail.drain(&bwr)
	enc.codeTail.drain(&bwr)
	if !enc.backoutTail.empty() || !enc.codeTail.empty() {
		return bwr.wroteOctets(), false
	}

	bwr.padOut()
	return bwr.wroteOctets(), true
}
