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
// bits successfully written.  The wroteOctets field is updated to the number of complete octets written
// so far.
func (bwr *bitWriter) writeBits(src BitString, srcBitOffset int) (wroteBits int) {
	for bwr.di < len(bwr.dst) {
		packed, n := src.extract(srcBitOffset, 8)
		if n == 0 {
			break
		}

		// Conversion safety: 0 < bwr.lowAvail <= 8
		// Shift position 8 down to position bwr.lowAvail.
		bwr.dst[bwr.di] |= packed >> uint(8-bwr.lowAvail)
		bwr.lowAvail -= n

		if bwr.lowAvail > 0 {
			wroteBits += n
			break
		} else if bwr.di+1 < len(bwr.dst) {
			// Write the remaining top bits into next octet.
			bwr.lowAvail += 8
			// Conversion safety: bwr.lowAvail = (orig + (8 - n)); 0 < orig <= 8.
			// Shift position 8 - n up to position bwr.lowAvail.
			bwr.dst[bwr.di+1] = packed << uint(bwr.lowAvail-(8-n))
			wroteBits += n
		} else {
			// Don't count the unwritable -bwr.lowAvail bits.
			wroteBits += n + bwr.lowAvail
			bwr.lowAvail = 8
		}

		bwr.di++
		srcBitOffset += 8
	}

	return
}

// backOut backs out the output to the most recent complete octet and returns the number of bits backed over.
func (bwr *bitWriter) backOut() (backBits int) {
	if bwr.lowAvail == 8 {
		return
	} else {
		backBits = 8 - bwr.lowAvail
		bwr.lowAvail = 8
		return
	}
}

// padOut pads the output to the next complete octet.  It always succeeds, because the destination must have
// already had enough space for a full octet if a partial octet has been written.
func (bwr *bitWriter) padOut() {
	// If there's eight low bits available, we're already positioned at the beginning of an octet.
	if bwr.lowAvail < 8 {
		bwr.lowAvail = 8
		bwr.di++
	}
}

func (bwr bitWriter) aligned() bool {
	return bwr.lowAvail == 8
}

func (bwr bitWriter) wroteOctets() int {
	return bwr.di
}

// Encoder holds state for a single streaming Huffman encoding, converting one byte stream into another.
type Encoder struct {
	coding        *Coding
	anyHeldSymbol bool
	heldSymbol    symbol
	heldBitOffset int
}

// NewEncoder constructs a stateful encoder for the given coding.
func NewEncoder(coding *Coding) *Encoder {
	return &Encoder{
		coding: coding,
	}
}

func (enc *Encoder) writeHeld(bwr *bitWriter) {
	codeTable := enc.coding.codeTable

	if enc.anyHeldSymbol {
		code := codeTable[uint8(enc.heldSymbol)]
		wroteBits := bwr.writeBits(code, enc.heldBitOffset)
		enc.heldBitOffset += wroteBits
		if enc.heldBitOffset == code.BitLength {
			enc.anyHeldSymbol = false
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
	if enc.anyHeldSymbol || len(src) == 0 {
		return bwr.wroteOctets(), 0
	}

	si := 0
	for {
		sym := symbol(src[si])
		code := codeTable[uint8(sym)]
		si++

		wroteBits := bwr.writeBits(code, 0)
		if wroteBits < code.BitLength {
			enc.anyHeldSymbol = true
			enc.heldSymbol = sym
			enc.heldBitOffset = wroteBits
			break
		}

		if si == len(src) {
			if backBits := bwr.backOut(); backBits != 0 {
				enc.anyHeldSymbol = true
				enc.heldSymbol = sym
				enc.heldBitOffset = wroteBits - backBits
			}
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
	return !enc.anyHeldSymbol
}

// Flush pads the output with zero bits to the next byte and attempts to write any pending output into dst.
// It returns the number of bytes written into dst and whether or not all pending output has been successfully
// written.  Since this padding is not delimited, the resultant output stream ceases to be a consistent Huffman
// coded stream at this point; if resynchronization is necessary it must be done elsewhere.
func (enc *Encoder) Flush(dst []byte) (dn int, finished bool) {
	bwr := newBitWriter(dst)
	enc.writeHeld(&bwr)
	if enc.anyHeldSymbol {
		return bwr.wroteOctets(), false
	}

	bwr.padOut()
	return bwr.wroteOctets(), true
}
