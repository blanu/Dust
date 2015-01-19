package Dust

type ParameterErrorHow int

const (
	ParameterErrorUnknown ParameterErrorHow = iota
	ParameterMissing
	ParameterUnexpected
	ParameterInvalid
)

type ParameterError struct {
	How ParameterErrorHow
	Kind string
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
		str += " '"+pe.Specific+"'"
	}
	return str
}
