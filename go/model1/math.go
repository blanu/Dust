package model1

func clampUint16(n float64) uint16 {
	// Use positive test for in-range to handle NaN properly, just in case.
	switch {
	case 0.0 <= n && n <= 65535.0:
		return uint16(n)
	case 65535.0 < n:
		return 65535
	default:
		return 0
	}
}
