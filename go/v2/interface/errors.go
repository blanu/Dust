// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package Dust

// ParameterErrorHow describes whether the data element referenced in a ParameterError is missing, unexpected
// (present but without a known interpretation), or invalid (present when expected but with an uninterpretable
// value).
type ParameterErrorHow int

const (
	ParameterErrorUnknown ParameterErrorHow = iota
	ParameterMissing
	ParameterUnexpected
	ParameterInvalid
)

// ParameterError describes a problem relating to a specific data element.  Specific may be the empty string
// to refer to the single element of a kind.
type ParameterError struct {
	How      ParameterErrorHow
	Kind     string
	Specific string
}

func (pe *ParameterError) Error() string {
	var str string
	switch pe.How {
	case ParameterErrorUnknown:
		str = "??? "
	case ParameterMissing:
		str = "missing "
	case ParameterUnexpected:
		str = "unexpected "
	case ParameterInvalid:
		str = "invalid "
	}

	str += pe.Kind
	if pe.Specific != "" {
		str += " '" + pe.Specific + "'"
	}
	return str
}
