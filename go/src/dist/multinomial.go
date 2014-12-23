// Copyright ©2014 The gonum Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package dist

import (
	"math/rand"
)

// Poisson represents the Poisson distribution (https://en.wikipedia.org/wiki/Poisson_distribution).
type Multinomial struct {
	Weights []float64
	Source *rand.Rand
}

// Rand returns a random sample drawn from the distribution.
func (e Multinomial) Rand() uint64 {
	var rnd float64
	if e.Source == nil {
		rnd = rand.Float64()
	} else {
		rnd = e.Source.Float64()
	}

	var last float64 = 0
	var next float64 = 0

  for index, weight := range e.Weights {
		next=next+weight
		if rnd>last && rnd<next {
			return uint64(index)
		}
		last=next
	}

	return uint64(len(e.Weights))
}
