// Copyright ©2014 The gonum Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package dist

import (
	"math"
	"math/rand"
)

// Poisson represents the Poisson distribution (https://en.wikipedia.org/wiki/Poisson_distribution).
type Poisson struct {
	Expected   float64
	Source *rand.Rand
}

// Rand returns a random sample drawn from the distribution.
/* Poisson sampling according to Knuth
init:
  Let L ← e−λ, k ← 0 and p ← 1.
do:
	k ← k + 1.
	Generate uniform random number u in [0,1] and let p ← p × u.
	while p > L.
return k − 1.
*/
func (self Poisson) Rand() uint16 {
	var l float64 = math.E-self.Expected
	var k uint16 = 0

  for p := rand.Float64(); p > l; p = p * rand.Float64() {
		k=k+1
	}

	return k
}
