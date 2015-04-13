// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

/*
Package buf contains a few imperative buffer-handling utility routines to avoid mistake-prone
repetition when streaming and reassembling chunks of bytes.
*/
package buf

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

// BeginReassemblyArray returns an empty reassembly backed by the given array.
func BeginReassemblyArray(array []byte) Reassembly {
	return array[:0]
}

// ExistingReassemblyArray returns a reassembly backed by the given array, with n bytes already present.
func ExistingReassemblyArray(array []byte, n int) Reassembly {
	return array[:n]
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

func (reassembly *Reassembly) CopyIn(in []byte) int {
	return CopyReassemble(reassembly, &in)
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

func (reassembly *Reassembly) TransformIn(in []byte, transform func(dst []byte, src []byte)) int {
	return TransformReassemble(reassembly, &in, transform)
}

// FixedSizeComplete returns true iff the reassembly buffer is completely full; it is assumed
// to have been created with the correct fixed size for the expected data.
func (reassembly Reassembly) FixedSizeComplete() bool {
	return len(reassembly) == cap(reassembly)
}

// Data returns the slice of valid bytes in reassembly.  This is the same as slicing the reassembly.
func (reassembly Reassembly) Data() []byte {
	return reassembly
}

// ValidLen returns the number of valid bytes in reassembly.
func (reassembly Reassembly) ValidLen() int {
	return len(reassembly)
}

// Empty returns true iff reassembly contains no data.
func (reassembly Reassembly) Empty() bool {
	return len(reassembly) == 0
}

// Consume alters reassembly so that all but the first n bytes are copied to the front and become the
// new valid region.
func (reassembly *Reassembly) Consume(n int) {
	switch {
	case n > len(*reassembly):
		panic("bufman: consuming more bytes than are available in reassembly buffer")
	case n == len(*reassembly):
		*reassembly = (*reassembly)[:0]
	case n < len(*reassembly):
		remaining := copy(*reassembly, (*reassembly)[n:len(*reassembly)])
		*reassembly = (*reassembly)[:remaining]
	}
}

func (reassembly *Reassembly) CopyOut(out *[]byte) int {
	n := copy(*out, *reassembly)
	reassembly.Consume(n)
	*out = (*out)[n:]
	return n
}

func (reassembly *Reassembly) PreData(n int) (fill []byte) {
	avail := cap(*reassembly) - len(*reassembly)
	if avail < n {
		n = avail
	}

	newLen := len(*reassembly) + n
	fill = (*reassembly)[len(*reassembly):newLen]
	*reassembly = (*reassembly)[:newLen]
	return
}

// Reset alters reassembly to contain no valid bytes, reusing the same underlying slice.
func (reassembly *Reassembly) Reset() {
	*reassembly = (*reassembly)[:0]
}

// CopyNew returns a fresh slice containing the same bytes as an existing slice.
func CopyNew(slice []byte) []byte {
	out := make([]byte, len(slice))
	copy(out, slice)
	return out
}

// Zero sets all bytes of a slice to zero.
func Zero(slice []byte) {
	for i, _ := range slice {
		slice[i] = 0
	}
}

// TODO: versions of these funs that don't require addressable bits on the input side
