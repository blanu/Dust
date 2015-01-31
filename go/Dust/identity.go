package Dust

import (
	"net"
	"strings"

	"github.com/blanu/Dust/go/Dust/crypting"
	"github.com/blanu/Dust/go/Dust/cryptions"
)

const (
	bridgeParamPublicKey      string = "p"
	bridgeParamModel          string = "m"
	bridgeParamOptionalSuffix string = "?"

	magicLine = "!!Dust-Server-Private!!"
)

type endpointAddress struct {
	tcpAddr *net.TCPAddr
	idBytes []byte
}

type modelSpec struct {
	name   string
	params map[string]string
}

type endpointConfig struct {
	endpointAddress
	modelSpec
}

// BridgeLine represents a Tor-style bridge line in parsed-text form, with strings corresponding to opaque
// nickname, network address, and parameters.
type BridgeLine struct {
	Nickname string
	Address  string
	Params   map[string]string
}

// ServerPublic represents a server public identity, comprising a public key, model parameters, and network
// address.
type ServerPublic struct {
	endpointConfig
	nickname       string
	longtermPublic cryptions.PublicKey
}

// ServerPrivate represents a server private identity, comprising a private key, model parameters, and network
// address.
type ServerPrivate struct {
	endpointConfig
	nickname     string
	longtermPair cryptions.KeyPair
}

func (ms modelSpec) ReifyModel() (ShapingModel, error) {
	constructor, ok := registeredModels[ms.name]
	if !ok {
		return nil, ErrInvalidModelName
	}

	return constructor(ms.params)
}

// CheckUnackedParams ensures that all parameters in params are either acknowledged by being associated
// with a true value in ackedParams or are optional due to being suffixed with a question mark.  If any
// unacknowledged requisite parameters are present, it returns an appropriate error.
func CheckUnackedParams(params map[string]string, ackedParams map[string]bool) error {
	for key, _ := range params {
		if !ackedParams[key] && !strings.HasSuffix(key, bridgeParamOptionalSuffix) {
			return &ParameterError{ParameterUnexpected, "parameter", key}
		}
	}

	return nil
}

func (spub ServerPublic) cryptoPublic() *crypting.Public {
	return &crypting.Public{
		IdBytes:        spub.endpointAddress.idBytes,
		LongtermPublic: spub.longtermPublic,
	}
}

// Public returns a server public identity corresponding to the given server private identity.
func (spriv ServerPrivate) Public() ServerPublic {
	return ServerPublic{
		nickname:       spriv.nickname,
		endpointConfig: spriv.endpointConfig,
		longtermPublic: spriv.longtermPair.Public(),
	}
}

func (spriv ServerPrivate) cryptoPrivate() *crypting.Private {
	return &crypting.Private{
		IdBytes:      spriv.endpointAddress.idBytes,
		LongtermPair: spriv.longtermPair,
	}
}

// ListenAddr returns the TCP address on which a server corresponding to the given identity would normally
// listen.
func (spriv ServerPrivate) ListenAddr() *net.TCPAddr {
	return spriv.tcpAddr
}

// DialAddr returns the TCP address to which a client wishing to contact the given identity would normally
// connect.
func (spub ServerPublic) DialAddr() *net.TCPAddr {
	return spub.tcpAddr
}

// NewServerPrivateBridgeLine generates a new server private identity suitable for the given bridge line.
func NewServerPrivateBridgeLine(bline BridgeLine) (result *ServerPrivate, err error) {
	ackedParams := make(map[string]bool)
	endpointConfig, err := loadEndpointConfigBridgeLine(bline, ackedParams)
	if err != nil {
		return
	}

	err = CheckUnackedParams(bline.Params, ackedParams)
	if err != nil {
		return
	}

	keyPair, err := cryptions.NewKeyPair()
	if err != nil {
		return
	}

	result = &ServerPrivate{
		nickname:       bline.Nickname,
		endpointConfig: *endpointConfig,
		longtermPair:   keyPair,
	}
	return
}
