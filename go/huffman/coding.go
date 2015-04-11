// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package huffman

import (
	"errors"
	"fmt"
	"sort"
	"strings"
)

var (
	ErrCodeTableInconsistent = errors.New("huffman: inconsistent code table")
	ErrCodeTableWrongLength  = errors.New("huffman: code table not of expected length")
	ErrCodeEmpty             = errors.New("huffman: empty code specified")
)

const (
	treeShift        = 3
	treeFanout       = 1 << treeShift
	treeSelectorMask = (1 << treeShift) - 1
	treeIndexStart   = 0

	treePointerTypeDescend = 0x40
	treePointerTypeReturn  = 0x80
	treePointerTypeMask    = 0xc0
)

type treePointer struct {
	target  uint8
	consume uint8
}

func treeBranch(relTarget int) treePointer {
	if !(0 <= relTarget && relTarget < 256) {
		panic("huffman: bad relative target")
	}

	return treePointer{uint8(relTarget), treePointerTypeDescend | treeShift}
}

func treeLeaf(symbol symbol, consume int) treePointer {
	if !(0 <= consume && consume <= treeShift) {
		panic("huffman: bad number of bits to consume")
	}

	return treePointer{uint8(symbol), treePointerTypeReturn | uint8(consume)}
}

type treeNode struct {
	children [treeFanout]treePointer
}

func (node treeNode) String() string {
	var parts []string

	for i := 0; i < treeFanout; i++ {
		pointer := node.children[i]

		var part string

		switch pointer.consume & treePointerTypeMask {
		default:
			part = "???"
		case treePointerTypeDescend:
			part = fmt.Sprintf("+%d", int(pointer.target))
		case treePointerTypeReturn:
			part = fmt.Sprintf("@%02x", uint8(pointer.target))
		}

		consume := pointer.consume &^ treePointerTypeMask
		if consume < treeShift {
			part = fmt.Sprintf("%d*%s", 1<<uint(treeShift-consume), part)
		}

		parts = append(parts, part)
		if consume < treeShift {
			i += 1<<uint(treeShift-consume) - 1
		}
	}

	return "[" + strings.Join(parts, " ") + "]"
}

// Coding holds the internal representation of a table mapping between Huffman codewords and eight-bit
// symbols.  A Coding can only be constructed given a consistent code table.
type Coding struct {
	codeTable []BitString
	treeNodes []treeNode
}

type codeScanning struct {
	codes   []BitString
	symbols []symbol
}

func (scan codeScanning) Len() int {
	return totalSymbols
}

func (scan codeScanning) Less(i, j int) bool {
	codeI, codeJ := scan.codes[i], scan.codes[j]

	for k := 0; k < codeI.BitLength && k < codeJ.BitLength; k += 8 {
		byteI, byteJ := codeI.Packed[k/8], codeJ.Packed[k/8]
		switch {
		case byteI < byteJ:
			return true
		case byteI > byteJ:
			return false
		default:
			// Continue loop.
		}
	}

	// If the codes had the same number of packed octets, the octets must have all been equal to get here.
	return codeI.BitLength < codeJ.BitLength
}

func (scan codeScanning) Swap(i, j int) {
	scan.codes[i], scan.codes[j] = scan.codes[j], scan.codes[i]
	scan.symbols[i], scan.symbols[j] = scan.symbols[j], scan.symbols[i]
}

func (scan codeScanning) makeSubtree(
	low, high int,
	bitOffset int,
) []treeNode {
	var lastIndex int = low
	var lastBits uint8 = 255
	var subNodes []treeNode = make([]treeNode, 1)
	var newNode treeNode

	maybeExtend := func(upto int) {
		switch {
		case lastIndex < upto:
			addNodes := scan.makeSubtree(lastIndex, upto, bitOffset+treeShift)
			newNode.children[lastBits] = treeBranch(len(subNodes))
			subNodes = append(subNodes, addNodes...)
		case lastIndex > upto:
			panic("huffman: bad indexing while scanning code table")
		}

		lastIndex, lastBits = upto, lastBits+1
	}

	for index := low; index < high; index++ {
		// We try to read one extra bit to detect end-of-string conveniently.
		bitsp1, count := scan.codes[index].extract(bitOffset, treeShift+1)
		bits := bitsp1 >> 1

		if count <= treeShift {
			maybeExtend(index)
			if bits != lastBits {
				panic(ErrCodeTableInconsistent)
			}

			leafFanout := uint8(1) << uint(treeShift-count)
			childPointer := treeLeaf(scan.symbols[index], count)
			for leafTail := uint8(0); leafTail < leafFanout; leafTail++ {
				newNode.children[bits+leafTail] = childPointer
			}

			lastIndex, lastBits = index+1, bits+leafFanout-1
		} else {
			if bits == lastBits {
				continue
			} else if bits != lastBits+1 {
				panic(ErrCodeTableInconsistent)
			}

			maybeExtend(index)
		}
	}

	maybeExtend(high)
	if lastBits != 1<<treeShift {
		panic(ErrCodeTableInconsistent)
	}

	subNodes[0] = newNode
	return subNodes
}

func makeTree(codeTable []BitString) (result []treeNode, err error) {
	codes := make([]BitString, len(codeTable))
	_ = copy(codes, codeTable)
	symbols := make([]symbol, 256)
	for i := 0; i < 256; i++ {
		symbols[i] = symbol(i)
	}

	scan := codeScanning{codes, symbols}
	sort.Sort(scan)

	defer func() {
		if panicked := recover(); panicked != nil {
			switch panicked {
			case ErrCodeTableInconsistent:
				result = nil
				err = panicked.(error)
				return
			default:
				panic(panicked)
			}
		}
	}()

	result = scan.makeSubtree(0, totalSymbols, 0)
	return
}

// NewCoding constructs a Coding from the given code table, or returns an error if the code table is
// inconsistent with Huffman modeling.  codeTable must have exactly 256 BitString entries.  codeTable[S] is
// the codeword for symbol S.  The set of codewords must be prefix-free, and each codeword must conform to
// BitString invariants.
func NewCoding(codeTable []BitString) (*Coding, error) {
	if len(codeTable) != totalSymbols {
		return nil, ErrCodeTableWrongLength
	}

	for _, bs := range codeTable {
		bs.check()

		if bs.BitLength == 0 {
			return nil, ErrCodeEmpty
		}
	}

	treeNodes, err := makeTree(codeTable)
	if err != nil {
		return nil, err
	}

	return &Coding{codeTable, treeNodes}, nil
}

func (coding Coding) TreeString() string {
	var parts []string

	for i, node := range coding.treeNodes {
		parts = append(parts, fmt.Sprintf("\t%d: %v\n", i, node))
	}

	return "TREE{\n" + strings.Join(parts, "") + "}"
}
