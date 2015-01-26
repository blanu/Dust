package huffman

type Decoder struct {
	coding *Coding
	treeIndex int
	heldBits uint32
	heldCount uint8
}

func NewDecoder(coding *Coding) *Decoder {
	return &Decoder{
		coding: coding,
		treeIndex: treeIndexStart,
	}
}

func (dec *Decoder) writeSymbolsTo(dst []byte) (dn int) {
	treeNodes := dec.coding.treeNodes

	for dn < len(dst) && 0 < dec.heldCount {
		var selector uint32
		if dec.heldCount >= treeShift {
			selector = (dec.heldBits >> (dec.heldCount - treeShift)) & treeSelectorMask
		} else {
			selector = (dec.heldBits << (treeShift - dec.heldCount)) & treeSelectorMask
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

func (dec *Decoder) Decode(dst, src []byte) (dn, sn int) {
	for {
		if dn == len(dst) {
			return
		}
		dn += dec.writeSymbolsTo(dst[dn:])

		if sn == len(src) {
			return 
		}
		for sn < len(src) && (32 - dec.heldCount) >= 8 {
			dec.heldBits = dec.heldBits << 8 | uint32(src[sn])
			dec.heldCount += 8
			sn++
		}
	}
}

func (dec *Decoder) Aligned() bool {
	return dec.heldBits == 0
}

func (dec *Decoder) Flush(dst []byte) (dn int, finished bool) {
	dn = dec.writeSymbolsTo(dst)
	finished = (dec.heldBits == 0)
	return
}
