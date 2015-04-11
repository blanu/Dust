// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package crypting

import (
	"errors"
)

type Params struct {
	// Maximum datagram size.
	MTU int

	// Whether to allow propagating backpressure by buffering unsent datagrams.
	HoldIncoming bool
}

const (
	MinMTU = 1280
	MaxMTU = 32000
)

var (
	ErrBadMTU = errors.New("Dust/crypting: bad MTU")
)

func (params *Params) Validate() error {
	if !(MinMTU <= params.MTU && params.MTU <= MaxMTU) {
		return ErrBadMTU
	}

	return nil
}
