package Dust

import (
	"github.com/blanu/Dust/go/Dust/shaping"
)

type (
	ShapingEncoder shaping.Encoder
	ShapingDecoder shaping.Decoder
)

type ShapingCodec interface {
	ShapingEncoder
	ShapingDecoder
}

type ShapingModel interface {
	MakeClientPair() (ShapingEncoder, ShapingDecoder, error)
	MakeServerPair() (ShapingEncoder, ShapingDecoder, error)
}

type ShapingModelConstructor func(params map[string]string) (ShapingModel, error)

var registeredModels = make(map[string]ShapingModelConstructor)

func RegisterModel(name string, constructor ShapingModelConstructor) {
	registeredModels[name] = constructor
}
