// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package huffman

// Decoder holds state for a single streaming Huffman decoding, converting one byte stream into another.
type Decoder struct {
	coding    *Coding
	treeIndex int
	heldWord  uint32
	heldCount uint8
}

// NewDecoder constructs a stateful decoder for the given coding.
func NewDecoder(coding *Coding) *Decoder {
	return &Decoder{
		coding:    coding,
		treeIndex: treeIndexStart,
	}
}

func (dec *Decoder) writeSymbolsTo(dst []byte) (dn int) {
	treeNodes := dec.coding.treeNodes

	for dn < len(dst) && 0 < dec.heldCount {
		var selector uint32
		if dec.heldCount >= treeShift {
			selector = (dec.heldWord >> (dec.heldCount - treeShift)) & treeSelectorMask
		} else {
			selector = (dec.heldWord << (treeShift - dec.heldCount)) & treeSelectorMask
		}

		pointer := treeNodes[dec.treeIndex].children[selector]
		consume := pointer.consume &^ treePointerTypeMask
		if dec.heldCount < consume {
			return
		}

		dec.heldCount -= consume
		switch pointer.consume & treePointerTypeMask {
		default:
			panic("huffman: weird tree pointer")
		case treePointerTypeDescend:
			dec.treeIndex += int(pointer.target)
		case treePointerTypeReturn:
			dst[dn] = uint8(pointer.target)
			dn++
			dec.treeIndex = treeIndexStart
		}
	}

	return
}

// Decode continues decoding bytes from src into dst, stopping when either no further source bytes can be
// consumed or no further destination bytes can be written.  It returns the number of bytes written to dst and
// consumed from src, respectively.
func (dec *Decoder) Decode(dst, src []byte) (dn, sn int) {
	for {
		if dn == len(dst) {
			return
		}
		dn += dec.writeSymbolsTo(dst[dn:])

		if sn == len(src) {
			return
		}
		for sn < len(src) && (32-dec.heldCount) >= 8 {
			dec.heldWord = dec.heldWord<<8 | uint32(src[sn])
			dec.heldCount += 8
			sn++
		}
	}
}

// Aligned returns true iff the input consumed so far corresponds exactly to the output produced so far.
func (dec *Decoder) Aligned() bool {
	return dec.heldCount == 0
}

// Flush attempts to write any pending symbols into dst.  It returns the number of bytes written into dst and
// whether or not the total input consumed corresponds exactly to the output.
func (dec *Decoder) Flush(dst []byte) (dn int, finished bool) {
	// TODO: this should really return whether all _symbols so far_ from the input have been written to
	// the output, but that implies some restructuring of writeSymbolsTo...
	dn = dec.writeSymbolsTo(dst)
	finished = (dec.heldCount == 0)
	return
}
