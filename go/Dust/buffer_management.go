package Dust

// Copy as many bytes as possible from *in to *out, and advance the start pointers of both *in and *out past
// the copied bytes.
func copyAdvance(out *[]byte, in *[]byte) int {
	n := copy(*out, *in)
	*in = (*in)[n:]
	*out = (*out)[n:]
	return n
}

// reassembly holds a slice of the valid part of a dedicated array into which new bytes will be appended
// at the end until some condition is met.  The length of the slice is the valid length, therefore, and
// the capacity is the maximum.
type reassembly []byte

// beginReassembly returns a fresh, empty reassembly with the given capacity.
func beginReassembly(size int) reassembly {
	return make([]byte, 0, size)
}

// copyReassemble copies bytes from *in to the blank portion of *out.  It updates *out to reflect the
// new valid length, sets *in to the unconsumed tail, and returns the number of bytes copied.
func copyReassemble(out *reassembly, in *[]byte) int {
	avail := (*out)[len(*out):cap(*out)]
	n := copy(avail, *in)
	*in = (*in)[n:]
	*out = (*out)[:len(*out)+n]
	return n
}

// transformReassemble is like copyReassemble, but uses a transformation function from a slice to a slice of
// equal length on the data.
func transformReassemble(
	out *reassembly, in *[]byte,
	transform func(dst []byte, src []byte),
) int {
	avail := (*out)[len(*out):cap(*out)]
	n := len(*in)
	if len(avail) < n {
		n = len(avail)
	}

	transform(avail[:n], (*in)[:n])
	*in = (*in)[n:]
	*out = (*out)[:len(*out)+n]
	return n
}

// fixedSizeReassemblyComplete returns true iff the reassembly buffer is completely full; it is assumed
// to have been created with the correct fixed size for the expected data.
func (reassembly reassembly) fixedSizeComplete() bool {
	return len(reassembly) == cap(reassembly)
}

// data returns the slice of valid bytes in reassembly.
func (reassembly reassembly) data() []byte {
	return reassembly
}

// validLen returns the number of valid bytes in reassembly.
func (reassembly reassembly) validLen() int {
	return len(reassembly)
}

// consume alters reassembly so that all but the first n bytes are copied to the front and become the
// new valid region.
func (reassembly *reassembly) consume(n int) {
	switch {
	case n > len(*reassembly):
		panic("consuming more bytes than are available in reassembly buffer")
	case n == len(*reassembly):
		*reassembly = (*reassembly)[:0]
	case n < len(*reassembly):
		remaining := copy(*reassembly, (*reassembly)[n:len(*reassembly)])
		*reassembly = (*reassembly)[:remaining]
	}
}

// copyNew returns a fresh slice containing the same bytes as an existing slice.
func copyNew(slice []byte) []byte {
	out := make([]byte, len(slice))
	copy(out, slice)
	return out
}
