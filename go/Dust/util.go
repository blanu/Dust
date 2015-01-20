package Dust

import (
	"fmt"
	"os"
)

type PanicError struct {
	Value interface{}
}

func (pe *PanicError) Error() string {
	return fmt.Sprintf("panic: %v", pe.Value)
}

func ReportExitTo(cell *error) {
	if panicked := recover(); panicked != nil {
		*cell = &PanicError{panicked}
	}
}

func debugf(control string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, control + "\n", args...)
}
