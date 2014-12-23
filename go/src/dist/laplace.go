// Copyright ©2014 The gonum Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package dist

import (
	"math"
	"math/rand"
)

// Laplace represents the Laplace distribution (https://en.wikipedia.org/wiki/Laplace_distribution).
type Laplace struct {
	Mu     float64 // Mean of the Laplace distribution
	Scale  float64 // Scale of the Laplace distribution
	Source *rand.Rand
}

// Rand returns a random sample drawn from the distribution.
func (l Laplace) Rand() float64 {
	var rnd float64
	if l.Source == nil {
		rnd = rand.Float64()
	} else {
		rnd = l.Source.Float64()
	}
	u := rnd - 0.5
	if u < 0 {
		return l.Mu + l.Scale*math.Log(1+2*u)
	}
	return l.Mu - l.Scale*math.Log(1-2*u)
}
