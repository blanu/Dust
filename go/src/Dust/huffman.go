package Dust

import (
  "bytes"
  "fmt"
  "strconv"
)

type HuffmanEncoder struct {
  tree *huffmanTree
  codes map[byte]code
}

type huffmanTree struct {
  left  *huffmanTree
  right *huffmanTree
  value byte
}

type code []bool

func newHuffmanEncoder() *HuffmanEncoder {
  return &HuffmanEncoder{tree: &huffmanTree{left: nil, right: nil, value: 0}, codes: map[byte]code{}}
}

func GenerateHuffmanEncoder(codes [][]bool) *HuffmanEncoder {
  var encoder=newHuffmanEncoder()
  for index, value := range codes {
    encoder.codes[byte(index)]=value
    encoder.insert(value, byte(index))
  }

  return encoder
}

func (self *HuffmanEncoder) insert(code []bool, value byte) {
  var branch=self.find(code)
  if branch.left!=nil {
    print("Error! Target branch was not empty")
    return
  }
  if branch.right!=nil {
    print("Error! Target branch was not empty")
    return
  }
  branch.value=value
}

func (self *HuffmanEncoder) find(code []bool) *huffmanTree {
  var branch=self.tree
  for _, value := range code {
    if value {
      if branch.left==nil {
        branch.left=&huffmanTree{left: nil, right: nil, value: 0}
      }
      branch=branch.left
    } else {
      if branch.right==nil {
        branch.right=&huffmanTree{left: nil, right: nil, value: 0}
      }
      branch=branch.right
    }
  }

  return branch
}

func (self *HuffmanEncoder) lookup(code []bool) byte {
  var branch=self.find(code)
  if branch.left!=nil {
    print("Error! Target branch was not empty")
    return 0
  }
  if branch.right!=nil {
    print("Error! Target branch was not empty")
    return 0
  }
  return branch.value
}

func (self *HuffmanEncoder) String() string {
  var buffer bytes.Buffer

  buffer.WriteString("HuffmanEncoder (\n")
  for index, value := range self.codes {
    buffer.WriteString("  "+strconv.Itoa(int(index))+" => "+value.String())
  }
  buffer.WriteString(")\n")

  return buffer.String()
}

func (self code) String() string {
  var buffer bytes.Buffer

  for _, value := range self {
    if value {
      buffer.WriteString("1")
    } else {
      buffer.WriteString("0")
    }
  }

  return buffer.String()
}

func (self *HuffmanEncoder) Encode(bytes []byte) (encoded []byte, buffer []byte, err error) {
  var bits = bytesToBits(bytes)

  return self.encodeBits(bits)
}

func (self *HuffmanEncoder) Decode(bytes []byte) (decoded []byte, buffer []byte, err error) {
  fmt.Printf("Decoding %d bytes\n", len(bytes))
  var bits = self.decodeBits(bytes)
  fmt.Printf("Decoded %d bits\n", len(bits))
  fmt.Printf("Bits: %s\n", code(bits).String())
  var result=bitsToBytes(bits)
  fmt.Printf("Decoded %d bytes\n", len(result))
  return result
}

func (self *HuffmanEncoder) encodeBits(bits []bool) []byte {
  var bytes = make([]byte, 0)
  var start *huffmanTree = self.tree
  var branch *huffmanTree = start
  for _, bit := range bits {
    if bit {
      branch=branch.left
    } else {
      branch=branch.right
    }

    if branch==nil {
      println("Error, path lead to a dead branch")
    } else if branch.left==nil && branch.right==nil {
      bytes=append(bytes, branch.value)
      branch=start
    } // else continue on path with next iteration
  }

  return bytes
}

func (self *HuffmanEncoder) decodeBits(bytes []byte) []bool {
  var bits=make([]bool, 0)
  for _, b := range bytes {
    var code=self.codes[b]
    for _, bit := range code {
      bits=append(bits, bit)
    }
  }

  return bits
}

func bytesToBits(bytes []byte) []bool {
  var bits = make([]bool, 0)
  for _, b := range bytes {
    for x:=0; x<8; x++ {
      var test byte = byte(1 << uint(x))
      bits=append(bits, (b & test)>0)
    }
  }
  return bits
}

func bitsToBytes(bits []bool) []byte {
  var bytes = make([]byte, 0)
  var b byte=0
  for index, bit := range bits {
    var ibit byte
    if bit {
      ibit=1
    } else {
      ibit=0
    }

    var offset=uint(index%8)
    b=b | byte(ibit<<offset)
    if offset==7 {
      bytes=append(bytes, b)
      b=0
    }
  }

  return bytes
}
