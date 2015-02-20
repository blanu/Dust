package proc

import (
	"io"
)

type ManagedCopy struct {
	out io.Writer
	in  io.Reader
}
