// Copyright ©2014 The gonum Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package dist

import (
	"math/rand"
)

// UnitNormal is an instantiation of the standard normal distribution
var UnitNormal = Normal{Mu: 0, Sigma: 1}

// Normal respresents a normal (Gaussian) distribution (https://en.wikipedia.org/wiki/Normal_distribution).
type Normal struct {
	Mu     float64 // Mean of the normal distribution
	Sigma  float64 // Standard deviation of the normal distribution
	Source *rand.Rand

	// Needs to be Mu and Sigma and not Mean and StdDev because Normal has functions
	// Mean and StdDev
}

// Rand returns a random sample drawn from the distribution.
func (n Normal) Rand() float64 {
	var rnd float64
	if n.Source == nil {
		rnd = rand.NormFloat64()
	} else {
		rnd = n.Source.NormFloat64()
	}
	return rnd*n.Sigma + n.Mu
}
