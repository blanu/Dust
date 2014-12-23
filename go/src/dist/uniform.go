// Copyright ©2014 The gonum Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package dist

import (
	"math/rand"
)

// Uniform represents a continuous uniform distribution (https://en.wikipedia.org/wiki/Uniform_distribution_%28continuous%29).
type Uniform struct {
	Min    float64
	Max    float64
	Source *rand.Rand
}

// Rand returns a random sample drawn from the distribution.
func (u Uniform) Rand() float64 {
	var rnd float64
	if u.Source == nil {
		rnd = rand.Float64()
	} else {
		rnd = u.Source.Float64()
	}
	return rnd*(u.Max-u.Min) + u.Min
}
