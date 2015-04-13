// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package Dust

import (
	"github.com/blanu/Dust/go/v2/crypting"
	"github.com/blanu/Dust/go/prim1"
)

// ModelSpec represents the name of a model to use for an endpoint, plus unparsed model-specific parameters.
type ModelSpec struct {
	Name   string
	Params map[string]string
}

// reifyModel tries to construct a live model from the spec based on the set of registered model constructors.
func (ms *ModelSpec) reifyModel() (ShapingModel, error) {
	constructor, ok := getModelConstructor(ms.Name)
	if !ok {
		return nil, ErrInvalidModelName
	}

	return constructor(ms.Params)
}

// EndpointParams represents all the preagreed parameters needed to establish the "same kind" of Dust
// connection between two endpoints, excluding endpoint-specific identity material.
type EndpointParams struct {
	ModelSpec ModelSpec
	Crypting  crypting.Params
}

var defEndpointParams = EndpointParams{
	Crypting: crypting.Params{
		MTU: 1500,
	},
}

// DefEndpointParams returns most of a 'stock' set of endpoint parameters.  The result is only missing
// a model spec.
func DefEndpointParams() EndpointParams {
	return defEndpointParams
}

// ServerPublic represents the public identity of a Dust server.
type ServerPublic struct {
	OpaqueId       crypting.OpaqueId
	LongtermPublic prim.Public
	EndpointParams
}

// ServerPrivate represents the private identity of a Dust server.
type ServerPrivate struct {
	OpaqueId        crypting.OpaqueId
	LongtermPrivate prim.Private
	EndpointParams
}

func (spub ServerPublic) cryptoPublic() *crypting.Public {
	return &crypting.Public{
		Id:  spub.OpaqueId,
		Key: spub.LongtermPublic,
	}
}

// Public returns a public identity corresponding to the given private identity.
func (spriv ServerPrivate) Public() *ServerPublic {
	return &ServerPublic{
		OpaqueId:       spriv.OpaqueId,
		LongtermPublic: spriv.LongtermPrivate.Public,
		EndpointParams: spriv.EndpointParams,
	}
}

func (spriv ServerPrivate) cryptoPrivate() *crypting.Private {
	return &crypting.Private{
		Id:  spriv.OpaqueId,
		Key: spriv.LongtermPrivate,
	}
}

// NewServerPrivate generates a new server private identity suitable for the given endpoint parameters.  ep
// may not be nil.
func NewServerPrivate(ep *EndpointParams) (result *ServerPrivate, err error) {
	private := prim.NewPrivate()
	opaque := crypting.NewOpaqueId()
	return &ServerPrivate{
		OpaqueId:        *opaque,
		LongtermPrivate: private,
		EndpointParams:  *ep,
	}, nil
}
