// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package Dust

import (
	"sync"

	"github.com/blanu/Dust/go/v2/shaping"
)

// See the shaping package for the definitions of Encoder and Decoder.
type (
	ShapingEncoder shaping.Encoder
	ShapingDecoder shaping.Decoder
)

// ShapingCodec is a convenience type mostly for internal use by model implementations.  It is not required to
// be used anywhere.
type ShapingCodec interface {
	ShapingEncoder
	ShapingDecoder
}

// ShapingModel provides a way of initializing encoder/decoder pairs for either side of a Dust connection
// using the model.  For parameterless models, the underlying type may hold no data, or may hold precalculated
// tables or anything else useful.  Two methods are needed because the two half-connections from a shared model
// may have asymmetrical shaping characteristics.
type ShapingModel interface {
	MakeClientPair() (ShapingEncoder, ShapingDecoder, error)
	MakeServerPair() (ShapingEncoder, ShapingDecoder, error)
}

// ShapingModelConstructor takes a map of parameters and builds a suitable ShapingModel from which encoders
// and decoders can be initialized.  Many models will not use any parameters.  The constructor must return a
// suitable error if it encounters any unrecognized requisite parameters, checking for them using the
// semantics of CheckUnackedParams.
type ShapingModelConstructor func(params map[string]string) (ShapingModel, error)

var registeredModels = make(map[string]ShapingModelConstructor)
var registeredModelMutex sync.Mutex

// RegisterModel registers that models of the type named by name can be constructed by using constructor.  It
// panics if a model constructor of this name is already registered.
func RegisterModel(name string, constructor ShapingModelConstructor) {
	registeredModelMutex.Lock()
	defer registeredModelMutex.Unlock()
	_, already := registeredModels[name]
	if already {
		panic("Dust: registering model '" + name + "' twice")
	}

	registeredModels[name] = constructor
}

// ModelsAvailable returns a list of all registered model type names, in no particular order.
func ModelsAvailable() []string {
	registeredModelMutex.Lock()
	defer registeredModelMutex.Unlock()

	models := []string{}
	for name, _ := range registeredModels {
		models = append(models, name)
	}
	return models
}

// ModelAvailable returns true iff a shaping model with the given name has been registered with the Dust
// package.
func ModelAvailable(name string) bool {
	registeredModelMutex.Lock()
	defer registeredModelMutex.Unlock()

	_, ok := registeredModels[name]
	return ok
}

func getModelConstructor(name string) (result ShapingModelConstructor, ok bool) {
	registeredModelMutex.Lock()
	defer registeredModelMutex.Unlock()
	result, ok = registeredModels[name]
	return
}
