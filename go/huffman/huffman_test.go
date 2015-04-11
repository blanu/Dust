// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package huffman_test

import (
	"bytes"
	"fmt"
	"math/rand"
	"strings"
	"testing"

	"github.com/blanu/Dust/go/DustModel/huffman"
)

var _ = huffman.NewCoding
var _ = fmt.Printf

type treeNode struct {
	symbol      int
	left, right *treeNode
}

const (
	randSeed   = 0x5a025ca11825a5e7
	iterations = 10
)

var rng *rand.Rand

func randomHuffmanTree() *treeNode {
	nodes := make([]*treeNode, 256)
	for i, _ := range nodes {
		nodes[i] = &treeNode{i, nil, nil}
	}

	var swap int
	for len(nodes) >= 2 {
		swap = rng.Intn(len(nodes))
		nodes[0], nodes[swap] = nodes[swap], nodes[0]
		swap = 1 + rng.Intn(len(nodes)-1)
		nodes[1], nodes[swap] = nodes[swap], nodes[1]
		nodes[1] = &treeNode{-1, nodes[0], nodes[1]}
		nodes = nodes[1:]
	}

	return nodes[0]
}

func plusOneBit(bs huffman.BitString, tail uint8) huffman.BitString {
	packed := make([]byte, (bs.BitLength+8)/8)
	copy(packed, bs.Packed)
	packed[bs.BitLength/8] |= tail << uint(7-bs.BitLength%8)
	return huffman.BitString{packed, bs.BitLength + 1}
}

func (node treeNode) writeToCodeTable(table []huffman.BitString, prefix huffman.BitString) {
	if node.symbol >= 0 {
		table[node.symbol] = prefix
	} else {
		node.left.writeToCodeTable(table, plusOneBit(prefix, 0))
		node.right.writeToCodeTable(table, plusOneBit(prefix, 1))
	}
}

func randomCodeTable() []huffman.BitString {
	codeTable := make([]huffman.BitString, 256)
	tree := randomHuffmanTree()
	tree.writeToCodeTable(codeTable, huffman.BitString{})
	return codeTable
}

func TestRandomCodings(t *testing.T) {
	rng = rand.New(rand.NewSource(randSeed))

	for iteration := 0; iteration < iterations; iteration++ {
		codeTable := randomCodeTable()
		coding, err := huffman.NewCoding(codeTable)
		if err != nil {
			t.Errorf("valid coding #%d got: %v", iteration, err)
			continue
		}
		_ = coding
	}
}

func transcodeLoop(
	t *testing.T,
	dst *[]byte, src []byte,
	xfer func(dst, src []byte) (dn, sn int),
	flush func(dst []byte) (dn int, complete bool),
) {
	dn, sn := 0, 0
	maybeExpand := func(delta int) {
		if dn == len(*dst) {
			reallocated := make([]byte, len(*dst)+delta)
			copy(reallocated, *dst)
			*dst = reallocated
		}
	}

	for sn < len(src) {
		maybeExpand(1 + len(*dst))
		dsubn, ssubn := xfer((*dst)[dn:], src[sn:])
		if dsubn == 0 && ssubn == 0 {
			t.Fatalf("no progress in transfer loop")
		}
		dn += dsubn
		sn += ssubn
	}

	for {
		maybeExpand(256)
		dsubn, finished := flush((*dst)[dn:])
		dn += dsubn
		if dn < len(*dst) || finished {
			break
		}
	}

	*dst = (*dst)[:dn]
}

func showBinaryOctets(b []byte) string {
	parts := make([]string, len(b))
	for i, x := range b {
		parts[i] = fmt.Sprintf("%08b", x)
	}
	return strings.Join(parts, " ")
}

func TestLoopback(t *testing.T) {
	rng = rand.New(rand.NewSource(randSeed))

	for iteration := 0; iteration < iterations; iteration++ {
		codeTable := randomCodeTable()
		coding, err := huffman.NewCoding(codeTable)
		if err != nil {
			t.Errorf("valid coding #%d got: %v", iteration, err)
			continue
		}

		enc := huffman.NewEncoder(coding)
		dec := huffman.NewDecoder(coding)

		dataLen := rng.Intn(10)
		dataIn := make([]byte, dataLen)
		for i, _ := range dataIn {
			dataIn[i] = uint8(rng.Int())
		}
		t.Logf("input: " + showBinaryOctets(dataIn))

		var huffed []byte
		transcodeLoop(t, &huffed, dataIn, enc.Encode, enc.Flush)
		t.Logf("huffed: " + showBinaryOctets(huffed))

		var dataOut []byte
		transcodeLoop(t, &dataOut, huffed, dec.Decode, dec.Flush)
		t.Logf("looped: " + showBinaryOctets(dataOut))

		// Since this is an unterminated Huffman coding, we may decode some garbage symbols at the
		// end.  TODO: check to make sure any garbage symbols come from zero-valued decode.
		lenOk := len(dataOut) >= len(dataIn)
		if !(lenOk && bytes.Equal(dataOut[:len(dataIn)], dataIn)) {
			t.Fatalf("coding #%d failed to loop around %d -> %d -> %d bytes of data",
				iteration, dataLen, len(huffed), len(dataOut))
			continue
		}
	}
}

// TODO:
//   - make sure it bombs on invalid codings
//   - make sure codings actually correspond to input code tables
//   - test edge cases of encode/decode functions explicitly
