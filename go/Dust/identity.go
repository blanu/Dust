package Dust

import (
	"bufio"
	"errors"
	"net"
	"os"
	"strconv"
	"strings"

	"github.com/blanu/Dust/go/Dust/crypting"
	"github.com/blanu/Dust/go/Dust/cryptions"
)

const (
	bridgeParamPublicKey      string = "p"
	bridgeParamModel          string = "m"
	bridgeParamOptionalPrefix string = "?"

	magicLine = "!!Dust-Server-Private!!"
)

var (
	ErrNoMagic          = &ParameterError{ParameterMissing, "magic line", ""}
	ErrNoAddress        = &ParameterError{ParameterMissing, "network address", ""}
	ErrNoPrivateKey     = &ParameterError{ParameterMissing, "private key", ""}
	ErrNoPublicKey      = &ParameterError{ParameterMissing, "public key", ""}
	ErrNoModelName      = &ParameterError{ParameterMissing, "model name", ""}
	ErrInvalidAddress   = &ParameterError{ParameterInvalid, "network address", ""}
	ErrInvalidModelName = &ParameterError{ParameterInvalid, "model name", ""}
	ErrSyntax           = errors.New("Dust: bad identity record syntax")
)

type endpointAddress struct {
	tcpAddr *net.TCPAddr
	idBytes []byte
}

type modelSpec struct {
	name   string
	params map[string]string
}

func (ms modelSpec) ReifyModel() (ShapingModel, error) {
	constructor, ok := registeredModels[ms.name]
	if !ok {
		return nil, ErrInvalidModelName
	}

	return constructor(ms.params)
}

type endpointConfig struct {
	endpointAddress
	modelSpec
}

// BridgeLine represents a Tor-style bridge line in parsed-text form, with a string corresponding to a network
// address and a string map of parameters.
type BridgeLine struct {
	Address string
	Params  map[string]string
}

// ServerPublic represents a server public identity, comprising a public key, model parameters, and network
// address.
type ServerPublic struct {
	endpointConfig
	longtermPublic cryptions.PublicKey
}

// ServerPrivate represents a server private identity, comprising a private key, model parameters, and network
// address.
type ServerPrivate struct {
	endpointConfig
	longtermPair cryptions.KeyPair
}

func parseEndpointAddress(addrString string) (*endpointAddress, error) {
	// Do all the splitting manually, because we really don't want to accidentally take a DNS lookup here.
	// Unfortunately, there's no net.ParseTCPAddr, only net.ResolveTCPAddr.
	colonIndex := strings.LastIndex(addrString, ":")
	if colonIndex == -1 {
		return nil, ErrInvalidAddress
	}

	hostString := addrString[:colonIndex]
	portString := addrString[colonIndex+1:]
	if strings.HasPrefix(hostString, "[") && strings.HasSuffix(hostString, "]") {
		hostString = hostString[1 : len(hostString)-1]
	} else if strings.IndexRune(hostString, ':') != -1 {
		// If the host part has a colon, require it to be bracketed.
		return nil, ErrInvalidAddress
	}

	ip := net.ParseIP(hostString)
	if ip == nil {
		return nil, ErrInvalidAddress
	}

	port, err := strconv.ParseUint(portString, 10, 16)
	if err != nil {
		return nil, ErrInvalidAddress
	}

	tcpAddr := &net.TCPAddr{ip, int(port), ""}
	idBytes, err := crypting.IdentityBytesOfNetworkAddress(tcpAddr)
	if err != nil {
		return nil, ErrInvalidAddress
	}

	return &endpointAddress{tcpAddr, idBytes}, nil
}

func extractModelSpec(
	params map[string]string,
	ackedParams map[string]bool,
	topKey string,
) (*modelSpec, error) {
	modelName, ok := params[topKey]
	if !ok {
		return nil, ErrNoModelName
	}
	ackedParams[topKey] = true

	subprefix := topKey + "."
	modelParams := make(map[string]string)
	for key, val := range params {
		if strings.HasPrefix(key, subprefix) {
			modelParams[key[len(subprefix):]] = val
			ackedParams[key] = true
		}
	}

	return &modelSpec{modelName, modelParams}, nil
}

func insertModelSpec(ms *modelSpec, params map[string]string, topKey string) {
	params[topKey] = ms.name
	for subkey, val := range ms.params {
		params[topKey+"."+subkey] = val
	}
}

// CheckUnackedParams ensures that all parameters in params are either acknowledged by being associated
// with a true value in ackedParams or are optional due to being prefixed with a question mark.  If any
// unacknowledged requisite parameters are present, it returns an appropriate error.
func CheckUnackedParams(params map[string]string, ackedParams map[string]bool) error {
	for key, _ := range params {
		if !ackedParams[key] && !strings.HasPrefix(key, bridgeParamOptionalPrefix) {
			return &ParameterError{ParameterUnexpected, "parameter", key}
		}
	}

	return nil
}

func loadEndpointConfigBridgeLine(
	bline BridgeLine,
	ackedParams map[string]bool,
) (result *endpointConfig, err error) {
	endpointAddress, err := parseEndpointAddress(bline.Address)
	if err != nil {
		return
	}

	modelSpec, err := extractModelSpec(bline.Params, ackedParams, bridgeParamModel)
	if err != nil {
		return
	}

	result = &endpointConfig{
		endpointAddress: *endpointAddress,
		modelSpec:       *modelSpec,
	}
	return
}

// LoadServerPublicBridgeLine converts parameters from a bridge line into a server public identity.
func LoadServerPublicBridgeLine(bline BridgeLine) (result *ServerPublic, err error) {
	ackedParams := make(map[string]bool)
	endpointConfig, err := loadEndpointConfigBridgeLine(bline, ackedParams)
	if err != nil {
		return
	}

	publicString, ok := bline.Params[bridgeParamPublicKey]
	if !ok {
		return nil, ErrNoPublicKey
	}
	longtermPublic, err := cryptions.LoadPublicKeyBase32(publicString)
	if err != nil {
		return
	}
	ackedParams[bridgeParamPublicKey] = true

	err = CheckUnackedParams(bline.Params, ackedParams)
	if err != nil {
		return
	}

	result = &ServerPublic{
		endpointConfig: *endpointConfig,
		longtermPublic: longtermPublic,
	}
	return
}

// BridgeLine returns a suitable bridge line for a server public identity.
func (spub ServerPublic) BridgeLine() BridgeLine {
	addrString := spub.tcpAddr.String()
	params := map[string]string{
		bridgeParamPublicKey: spub.longtermPublic.Base32(),
	}
	insertModelSpec(&spub.modelSpec, params, "m")
	return BridgeLine{addrString, params}
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

// LoadServerPrivateFile loads server private identity information from path.
func LoadServerPrivateFile(
	path string,
) (result *ServerPrivate, err error) {
	var file *os.File
	defer func() {
		if file != nil {
			_ = file.Close()
		}
	}()

	if file, err = os.Open(path); err != nil {
		return
	}

	lines := bufio.NewScanner(file)
	scanErrorOr := func(otherError error) error {
		scanError := lines.Err()
		if scanError != nil {
			return scanError
		} else {
			return otherError
		}
	}

	if !lines.Scan() {
		return nil, scanErrorOr(ErrSyntax)
	}
	if lines.Text() != magicLine {
		return nil, ErrNoMagic
	}

	if !lines.Scan() {
		return nil, scanErrorOr(ErrNoAddress)
	}
	addrLine := lines.Text()
	endpointAddress, err := parseEndpointAddress(addrLine)
	if err != nil {
		return
	}

	if !lines.Scan() {
		return nil, scanErrorOr(ErrNoPrivateKey)
	}
	privateLine := lines.Text()

	var keyPair cryptions.KeyPair
	defer func() {
		if result == nil && keyPair != nil {
			keyPair.DestroyPrivate()
		}
	}()
	keyPair, err = cryptions.LoadPrivateKeyBase32(privateLine)
	if err != nil {
		return
	}

	params := make(map[string]string)
	for lines.Scan() {
		paramLine := lines.Text()
		equals := strings.IndexRune(paramLine, '=')
		if equals == -1 {
			return nil, ErrSyntax
		}

		key, val := paramLine[:equals], paramLine[equals+1:]
		params[key] = val
	}

	err = lines.Err()
	if err != nil {
		return
	}

	ackedParams := make(map[string]bool)
	modelSpec, err := extractModelSpec(params, ackedParams, bridgeParamModel)
	if err != nil {
		return
	}

	err = CheckUnackedParams(params, ackedParams)
	if err != nil {
		return
	}

	result = &ServerPrivate{
		endpointConfig: endpointConfig{
			endpointAddress: *endpointAddress,
			modelSpec:       *modelSpec,
		},
		longtermPair: keyPair,
	}
	return
}

// SavePrivateFile saves the given server private identity information to a new file named path.  The file
// must not already exist.
func (spriv ServerPrivate) SavePrivateFile(path string) error {
	headerLines := []string{
		magicLine,
		spriv.tcpAddr.String(),
		spriv.longtermPair.PrivateBase32(),
	}

	paramLines := []string{}
	for key, val := range spriv.Public().BridgeLine().Params {
		if strings.ContainsAny(key, "\r\n") || strings.ContainsAny(val, "\r\n") {
			return ErrSyntax
		}

		switch key {
		case bridgeParamPublicKey:
			// Don't save public key; it's inferred from the private key.
		default:
			paramLines = append(paramLines, key+"="+val)
		}
	}

	allLines := append([]string{}, headerLines...)
	allLines = append(allLines, paramLines...)
	allLines = append(allLines, "")
	contentString := strings.Join(allLines, "\n")

	var file *os.File
	var err error
	commit := false
	defer func() {
		if file != nil {
			_ = file.Close()
			if !commit {
				_ = os.Remove(path)
			}
		}
	}()

	if file, err = os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0600); err != nil {
		return err
	}

	if _, err = file.Write([]byte(contentString)); err != nil {
		return err
	}

	if err = file.Sync(); err != nil {
		return err
	}

	if err := file.Close(); err != nil {
		file = nil
		_ = os.Remove(path)
		return err
	}

	commit = true
	return nil
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
		endpointConfig: *endpointConfig,
		longtermPair:   keyPair,
	}
	return
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
