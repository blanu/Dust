package crypting

import (
	"errors"
)

type Params struct {
	// Maximum datagram size.
	MTU int
}

const (
	MinMTU = 1280
	MaxMTU = 32000
)

var (
	ErrBadMTU = errors.New("Dust/crypting: bad MTU")
)

func ValidateParams(params Params) error {
	if !(MinMTU <= params.MTU && params.MTU <= MaxMTU) {
		return ErrBadMTU
	}

	return nil
}
