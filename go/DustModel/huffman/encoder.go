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

// writeBits writes bits from src starting at srcBitOffset into the destination.  It returns the number of
// bits successfully written.
func (bwr *bitWriter) writeBits(src BitString, srcBitOffset int) (wroteBits int) {
	for bwr.di < len(bwr.dst) {
		packed, n := src.extract(srcBitOffset, 8)
		if n == 0 {
			break
		}
		srcBitOffset += 8

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
			// Don't count the unwritable -bwr.lowAvail bits.  Align to end of destination buffer.
			// We can't write any more.
			wroteBits += n - (-bwr.lowAvail)
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

// Encoder holds state for a single streaming Huffman encoding, converting one byte stream into another.
type Encoder struct {
	coding *Coding
	
	// held refers to heldStorage, which is mutated while encoding.  It can hold up to 8 pending bits.
	held        BitString
	heldStorage [1]byte
}

// NewEncoder constructs a stateful encoder for the given coding.
func NewEncoder(coding *Coding) (enc *Encoder) {
	enc = &Encoder{
		coding: coding,
	}

	enc.held.Packed = enc.heldStorage[:]
	enc.held.BitLength = 0
	return
}

func (enc *Encoder) writeHeld(bwr *bitWriter) {
	// bwr must be aligned already.
	if enc.held.BitLength > 0 {
		wroteBits := bwr.writeBits(enc.held, 0)
		switch {
		case wroteBits == enc.held.BitLength:
			enc.held.BitLength = 0
		case wroteBits == 0:
			// Couldn't write anything.
		default:
			// We should have had either a zero-length output buffer or a full octet available,
			// which would have been able to accept any held string.  Something is terribly wrong.
			panic("huffman: inconsistent partial writeHeld")
		}
	}
}

// Encode continues encoding bytes from src into dst, stopping when either no further source bytes can be
// consumed or no further destination bytes can be written.  It returns the number of bytes written to dst and
// consumed from src, respectively.
func (enc *Encoder) Encode(dst []byte, src []byte) (dn int, sn int) {
	codeTable := enc.coding.codeTable
	bwr := newBitWriter(dst)
	enc.writeHeld(&bwr)
	if enc.held.BitLength > 0 || len(src) == 0 {
		return bwr.wroteOctets(), 0
	}

	si := 0
	for {
		sym := symbol(src[si])
		si++
		code := codeTable[uint8(sym)]

		wroteBits := bwr.writeBits(code, 0)
		if wroteBits < code.BitLength || si == len(src) {
			enc.held.Packed[0], enc.held.BitLength = bwr.backOut()
			break
		}
	}

	if !bwr.aligned() {
		panic("huffman: weirdly misaligned bit writer")
	}

	return bwr.wroteOctets(), si
}

// Aligned returns true iff the input consumed so far corresponds exactly to the output produced so far or
// a successful Flush has been performed.
func (enc *Encoder) Aligned() bool {
	return enc.held.BitLength == 0
}

// Flush pads the output with zero bits to the next byte and attempts to write any pending output into dst.
// It returns the number of bytes written into dst and whether or not all pending output has been successfully
// written.  Since this padding is not delimited, the resultant output stream ceases to be a consistent Huffman
// coded stream at this point; if resynchronization is necessary it must be done elsewhere.
func (enc *Encoder) Flush(dst []byte) (dn int, finished bool) {
	bwr := newBitWriter(dst)
	enc.writeHeld(&bwr)
	if enc.held.BitLength > 0 {
		return bwr.wroteOctets(), false
	}

	bwr.padOut()
	return bwr.wroteOctets(), true
}
