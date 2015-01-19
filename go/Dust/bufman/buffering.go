package bufman

// Copy as many bytes as possible from *in to *out, and advance the start pointers of both *in and *out past
// the copied bytes.
func CopyAdvance(out *[]byte, in *[]byte) int {
	n := copy(*out, *in)
	*in = (*in)[n:]
	*out = (*out)[n:]
	return n
}

// A reassembly is a prefix of a dedicated array.  New bytes are generally appended between len() and cap(),
// advancing len(), until some condition is met.
type Reassembly []byte

// BeginReassembly returns a fresh, empty reassembly with the given capacity.
func BeginReassembly(size int) Reassembly {
	return make([]byte, 0, size)
}

// CopyReassemble copies bytes from *in to the blank portion of *out.  It updates *out to reflect the
// new valid length, sets *in to the unconsumed tail, and returns the number of bytes copied.
func CopyReassemble(out *Reassembly, in *[]byte) int {
	avail := (*out)[len(*out):cap(*out)]
	n := copy(avail, *in)
	*in = (*in)[n:]
	*out = (*out)[:len(*out)+n]
	return n
}

// TransformReassemble is like copyReassemble, but uses a transformation function from a slice to a slice of
// equal length on the data.
func TransformReassemble(
	out *Reassembly, in *[]byte,
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

// FixedSizeComplete returns true iff the reassembly buffer is completely full; it is assumed
// to have been created with the correct fixed size for the expected data.
func (reassembly Reassembly) FixedSizeComplete() bool {
	return len(reassembly) == cap(reassembly)
}

// Data returns the slice of valid bytes in reassembly.
func (reassembly Reassembly) Data() []byte {
	return reassembly
}

// ValidLen returns the number of valid bytes in reassembly.
func (reassembly Reassembly) ValidLen() int {
	return len(reassembly)
}

// Consume alters reassembly so that all but the first n bytes are copied to the front and become the
// new valid region.
func (reassembly *Reassembly) Consume(n int) {
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

// CopyNew returns a fresh slice containing the same bytes as an existing slice.
func CopyNew(slice []byte) []byte {
	out := make([]byte, len(slice))
	copy(out, slice)
	return out
}
