package Dust

type Distribution interface {
  random() uint64
}

type Exponential float64
type Poisson float64
type Normal struct {
  Mean float64
  Sd float64
}
type Multinomial []float64
