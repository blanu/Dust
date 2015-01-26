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

// A ShapingModelConstructor takes a map of parameters and builds a suitable ShapingModel from which encoders
// and decoders can be initialized.  Many models will not use any parameters.  The constructor must return a
// suitable error if it encounters any unrecognized requisite parameters, checking for them using the
// semantics of CheckUnackedParams.
type ShapingModelConstructor func(params map[string]string) (ShapingModel, error)

var registeredModels = make(map[string]ShapingModelConstructor)

// RegisterModel registers that models of the type named by name can be constructed by using constructor.
func RegisterModel(name string, constructor ShapingModelConstructor) {
	_, already := registeredModels[name]
	if already {
		panic("Dust: registering model '"+name+"' twice")
	}

	registeredModels[name] = constructor
}

// ModelsAvailable returns a list of all registered model type names, in no particular order.
func ModelsAvailable() []string {
	models := []string{}
	for name, _ := range registeredModels {
		models = append(models, name)
	}
	return models
}

// ModelAvailable returns true iff a shaping model with the given name has been registered with the Dust
// package.
func ModelAvailable(name string) bool {
	_, ok := registeredModels[name]
	return ok
}
