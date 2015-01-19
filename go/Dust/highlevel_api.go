/*
Package Dust implements the main Dust protocol codec for TCP/IP.

To use this library, use LoadServerPublicBridgeLine or LoadServerPrivateFile to acquire the
public or private side of a server identifier, respectively.  For each connection desired, construct a decoder
and encoder for the prearranged shaping model, then use BeginDustClient or BeginDustServer to establish a new
session, which you may use to exchange streams of octets.  Dust sessions will normally continue running in the
background for the duration provided by the model encoder.
*/
package Dust

import (
	"bufio"
	"errors"
	"net"
	"os"
	"strconv"
	"strings"
	
	. "github.com/blanu/Dust/go/DustCrypto"
)

const (
	bridgeParamPublicKey string = "p"
	bridgeParamC2SModel string = "cm"
	bridgeParamS2CModel string = "sm"
	bridgeParamOptionalPrefix string = "?"

	magicLine = "!!Dust-Server-Private!!"
)

var (
	ErrNoMagic = errors.New("magic line missing")
	ErrNoAddress = errors.New("no network address")
	ErrNoPrivateKey = errors.New("no private key")
	ErrNoPublicKey = errors.New("no public key")
	ErrNoModelName = errors.New("no model name")
	ErrInvalidAddress = errors.New("invalid network address")
	ErrInvalidParamLine = errors.New("invalid parameter line")
)

var registeredModels = make(map[string]ShapingModelConstructor)

func RegisterModel(name string, constructor ShapingModelConstructor) {
	registeredModels[name] = constructor
}

type endpointAddress struct {
	tcpAddr *net.TCPAddr
	idBytes []byte
}

type modelSpec struct {
	name string
	params map[string]string
}

type endpointConfig struct {
	endpointAddress
	c2sModel modelSpec
	s2cModel modelSpec
}

type BridgeLine struct {
	Address string
	Params map[string]string
}

type ServerPublic struct {
	endpointConfig
	longtermPublic PublicKey
}

type ServerPrivate struct {
	endpointConfig
	longtermPair KeyPair
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
		hostString = hostString[1:len(hostString)-1]
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
	idBytes, err := cryptoIdBytes(tcpAddr)
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

func extractBothModelSpecs(
	params map[string]string,
	ackedParams map[string]bool,
) (c2sModel *modelSpec, s2cModel *modelSpec, err error) {
	c2sModel, err = extractModelSpec(params, ackedParams, bridgeParamC2SModel)
	if err != nil {
		return
	}

	s2cModel, err = extractModelSpec(params, ackedParams, bridgeParamS2CModel)
	if err != nil {
		return
	}

	return
}

func insertModelSpec(ms *modelSpec, params map[string]string, topKey string) {
	params[topKey] = ms.name
	for subkey, val := range ms.params {
		params[topKey + "." + subkey] = val
	}
}

func insertBothModelSpecs(c2sModel *modelSpec, s2cModel *modelSpec, params map[string]string) {
	insertModelSpec(c2sModel, params, bridgeParamC2SModel)
	insertModelSpec(s2cModel, params, bridgeParamS2CModel)
}

func checkUnackedParams(params map[string]string, ackedParams map[string]bool) error {
	for key, _ := range params {
		if !ackedParams[key] && !strings.HasPrefix(key, bridgeParamOptionalPrefix) {
			// TODO: error type
			return errors.New("unrecognized parameter " + key)
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

	c2sModel, s2cModel, err := extractBothModelSpecs(bline.Params, ackedParams)
	if err != nil {
		return
	}

	result = &endpointConfig{
		endpointAddress: *endpointAddress,
		c2sModel: *c2sModel,
		s2cModel: *s2cModel,
	}
	return
}

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
	longtermPublic, err := LoadPublicKeyBase32(publicString)
	if err != nil {
		return
	}
	ackedParams[bridgeParamPublicKey] = true

	err = checkUnackedParams(bline.Params, ackedParams)
	if err != nil {
		return
	}

	result = &ServerPublic{
		endpointConfig: *endpointConfig,
		longtermPublic: longtermPublic,
	}
	return
}

func (spub ServerPublic) BridgeLine() BridgeLine {
	addrString := spub.tcpAddr.String()
	params := map[string]string{
		bridgeParamPublicKey: spub.longtermPublic.Base32(),
	}
	insertBothModelSpecs(&spub.c2sModel, &spub.s2cModel, params)
	return BridgeLine{addrString, params}
}

func (spriv ServerPrivate) Public() ServerPublic {
	return ServerPublic{
		endpointConfig: spriv.endpointConfig,
		longtermPublic: spriv.longtermPair.Public(),
	}
}

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
		return nil, scanErrorOr(ErrNoMagic)
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
	
	var keyPair KeyPair
	defer func() {
		if result == nil && keyPair != nil {
			keyPair.DestroyPrivate()
		}
	}()
	keyPair, err = LoadPrivateKeyBase32(privateLine)
	if err != nil {
		return
	}

	params := make(map[string]string)
	for lines.Scan() {
		paramLine := lines.Text()
		equals := strings.IndexRune(paramLine, '=')
		if equals == -1 {
			return nil, ErrInvalidParamLine
		}

		key, val := paramLine[:equals], paramLine[equals+1:]
		params[key] = val
	}

	err = lines.Err()
	if err != nil {
		return 
	}

	ackedParams := make(map[string]bool)
	c2sModel, s2cModel, err := extractBothModelSpecs(params, ackedParams)
	if err != nil {
		return
	}

	err = checkUnackedParams(params, ackedParams)
	if err != nil {
		return
	}

	result = &ServerPrivate{
		endpointConfig: endpointConfig{
			endpointAddress: *endpointAddress,
			c2sModel: *c2sModel,
			s2cModel: *s2cModel,
		},
		longtermPair: keyPair,
	}
	return
}

func (spriv ServerPrivate) SavePrivateFile(path string) error {
	headerLines := []string{
		magicLine,
		spriv.tcpAddr.String(),
		spriv.longtermPair.PrivateBase32(),
	}

	paramLines := []string{}
	for key, val := range spriv.Public().BridgeLine().Params {
		if strings.ContainsAny(key, "\r\n") || strings.ContainsAny(val, "\r\n") {
			// TODO: error type
			return errors.New("shouldn't have parameters with newlines")
		}

		switch key {
		case bridgeParamPublicKey:
			// Don't save public key; it's inferred from the private key.
		default:
			paramLines = append(paramLines, key + "=" + val)
		}
	}

	allLines := []string{}
	allLines = append(allLines, headerLines...)
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

	if file, err = os.OpenFile(path, os.O_WRONLY | os.O_CREATE | os.O_EXCL, 0600); err != nil {
		return err
	}
	
	if _, err = file.Write([]byte(contentString)); err != nil {
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

func NewServerPrivateBridgeLine(bline BridgeLine) (result *ServerPrivate, err error) {
	ackedParams := make(map[string]bool)
	endpointConfig, err := loadEndpointConfigBridgeLine(bline, ackedParams)
	if err != nil {
		return
	}

	err = checkUnackedParams(bline.Params, ackedParams)
	if err != nil {
		return
	}

	keyPair, err := NewKeyPair()
	if err != nil {
		return
	}

	result = &ServerPrivate{
		endpointConfig: *endpointConfig,
		longtermPair: keyPair,
	}
	return
}

func (spriv ServerPrivate) ListenAddr() *net.TCPAddr {
	return spriv.tcpAddr
}

func (spub ServerPublic) DialAddr() *net.TCPAddr {
	return spub.tcpAddr
}
