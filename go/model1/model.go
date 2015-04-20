package model1

import (
	"github.com/blanu/Dust/go/huffman"
)

type Rand interface {
	Rand() float64
}

type HalfStatic struct {
	Prefix    []byte
	HuffTable []huffman.BitString
}

type HalfModel struct {
	Prefix     []byte
	HuffCoding *huffman.Coding
}

func CompileHalf(static *HalfStatic) (compiled *HalfModel, err error) {
	compiled = &HalfModel{}
	compiled.Prefix = static.Prefix
	compiled.HuffCoding, err = huffman.NewCoding(static.HuffTable)
	return
}

func CompileTwoHalves(a, b *HalfStatic) (ax, bx *HalfModel, err error) {
	ax, err = CompileHalf(a)
	if err != nil {
		return
	}
	bx, err = CompileHalf(b)
	return
}

func CompileTwoHalvesOrPanic(a, b *HalfStatic, panicPrefix string) (ax, bx *HalfModel) {
	var err error
	ax, bx, err = CompileTwoHalves(a, b)
	if err != nil {
		panic(panicPrefix + err.Error())
	}
	return ax, bx
}
