// Copyright ©2014 The gonum Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package dist

import (
	"math/rand"
)

// Exponential represents the exponential distribution (https://en.wikipedia.org/wiki/Exponential_distribution).
type Exponential struct {
	Rate   float64
	Source *rand.Rand
}

// Rand returns a random sample drawn from the distribution.
func (e Exponential) Rand() float64 {
	var rnd float64
	if e.Source == nil {
		rnd = rand.ExpFloat64()
	} else {
		rnd = e.Source.ExpFloat64()
	}
	return rnd / e.Rate
}
