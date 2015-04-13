// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package Dust

import (
	"bufio"
	"errors"
	"os"
	"strconv"
	"strings"

	"github.com/blanu/Dust/go/v2/crypting"
	"github.com/blanu/Dust/go/prim1"
)

const (
	paramOpaqueId  = "n"
	paramPublicKey = "p"
	paramModel     = "m"
	paramMTU       = "mtu"

	paramPrivateKey = "px!"

	suffixOptional = "?"
)

var (
	ErrNoMagic          = &ParameterError{ParameterMissing, "magic line", ""}
	ErrNoOpaqueId       = &ParameterError{ParameterMissing, "opaque identifier", ""}
	ErrNoPrivateKey     = &ParameterError{ParameterMissing, "private key", ""}
	ErrNoPublicKey      = &ParameterError{ParameterMissing, "public key", ""}
	ErrNoModelName      = &ParameterError{ParameterMissing, "model name", ""}
	ErrInvalidModelName = &ParameterError{ParameterInvalid, "model name", ""}
	ErrSyntax           = errors.New("Dust: bad identity record syntax")
)

// CheckUnackedParams ensures that all parameters in params are either acknowledged by being associated
// with a true value in ackedParams or are optional due to being suffixed with a question mark.  If any
// unacknowledged requisite parameters are present, it returns an appropriate error.
func CheckUnackedParams(params map[string]string, ackedParams map[string]bool) error {
	for key, _ := range params {
		if !ackedParams[key] && !strings.HasSuffix(key, suffixOptional) {
			return &ParameterError{ParameterUnexpected, "parameter", key}
		}
	}

	return nil
}

type parseable interface {
	ParseFrom(unparsed map[string]string, acked map[string]bool) error
}

func parseNew(p parseable, unparsed map[string]string) (pp parseable, err error) {
	acked := make(map[string]bool)
	err = p.ParseFrom(unparsed, acked)
	if err != nil {
		return nil, err
	}

	err = CheckUnackedParams(unparsed, acked)
	if err != nil {
		return nil, err
	}

	return p, nil
}

func (ms *ModelSpec) ParseFrom(unparsed map[string]string, acked map[string]bool) error {
	topKey := paramModel
	name, ok := unparsed[topKey]
	if !ok {
		return ErrNoModelName
	}
	acked[topKey] = true

	subprefix := topKey + "."
	subparams := make(map[string]string)
	for key, val := range unparsed {
		if strings.HasPrefix(key, subprefix) {
			subparams[key[len(subprefix):]] = val
			acked[key] = true
		}
	}

	ms.Name = name
	ms.Params = subparams
	return nil
}

func (ms *ModelSpec) UnparseInto(unparsed map[string]string) {
	topKey := paramModel

	unparsed[topKey] = ms.Name
	for subkey, val := range ms.Params {
		unparsed[topKey+"."+subkey] = val
	}
}

func (ms *ModelSpec) Validate() error {
	_, err := ms.reifyModel()
	return err
}

func (ep *EndpointParams) ParseFrom(unparsed map[string]string, acked map[string]bool) (err error) {
	if err = ep.ModelSpec.ParseFrom(unparsed, acked); err != nil {
		return err
	}

	if mtuStr, present := unparsed[paramMTU]; present {
		var mtu uint64
		if mtu, err = strconv.ParseUint(mtuStr, 10, 0); err != nil {
			return
		}

		acked[paramMTU] = true
		ep.Crypting.MTU = int(mtu)
	}

	err = ep.Validate()
	return
}

func ParseEndpointParams(unparsed map[string]string) (result *EndpointParams, err error) {
	// Must explicitly copy here.
	ep := defEndpointParams
	resultp, err := parseNew(parseable(&ep), unparsed)
	if resultp != nil {
		result = resultp.(*EndpointParams)
	}
	return
}

func (ep *EndpointParams) UnparseInto(unparsed map[string]string) {
	def := &defEndpointParams

	ep.ModelSpec.UnparseInto(unparsed)

	if mtu := ep.Crypting.MTU; mtu != def.Crypting.MTU {
		unparsed[paramMTU] = strconv.FormatUint(uint64(mtu), 10)
	}

	// TODO: We don't store IgnoreDuration right now, assuming that to be forced by a transport
	// at the application layer.  Should we store it?
}

func (ep *EndpointParams) Validate() error {
	if err := ep.ModelSpec.Validate(); err != nil {
		return err
	}

	if err := ep.Crypting.Validate(); err != nil {
		return err
	}

	return nil
}

func (spub *ServerPublic) ParseFrom(unparsed map[string]string, acked map[string]bool) error {
	if err := spub.EndpointParams.ParseFrom(unparsed, acked); err != nil {
		return err
	}

	publicStr, present := unparsed[paramPublicKey]
	if !present {
		return ErrNoPublicKey
	}
	public, err := prim.LoadPublicText(publicStr)
	if err != nil {
		return err
	}
	acked[paramPublicKey] = true

	opaqueStr, present := unparsed[paramOpaqueId]
	if !present {
		return ErrNoOpaqueId
	}
	opaqueId, err := crypting.LoadOpaqueIdText(opaqueStr)
	if err != nil {
		return err
	}
	acked[paramOpaqueId] = true

	spub.LongtermPublic = public
	spub.OpaqueId = *opaqueId
	return nil
}

func ParseServerPublic(unparsed map[string]string) (result *ServerPublic, err error) {
	spub := &ServerPublic{EndpointParams: defEndpointParams}
	resultp, err := parseNew(parseable(spub), unparsed)
	if resultp != nil {
		result = resultp.(*ServerPublic)
	}
	return
}

func (spub *ServerPublic) UnparseInto(unparsed map[string]string) {
	unparsed[paramPublicKey] = spub.LongtermPublic.Text()
	unparsed[paramOpaqueId] = spub.OpaqueId.Text()
	spub.EndpointParams.UnparseInto(unparsed)
	return
}

func (spub *ServerPublic) Unparse() (unparsed map[string]string) {
	unparsed = make(map[string]string)
	spub.UnparseInto(unparsed)
	return
}

func (spriv *ServerPrivate) ParseFrom(unparsed map[string]string, acked map[string]bool) error {
	if err := spriv.EndpointParams.ParseFrom(unparsed, acked); err != nil {
		return err
	}

	privateStr, present := unparsed[paramPrivateKey]
	if !present {
		return ErrNoPrivateKey
	}
	private, err := prim.LoadPrivateText(privateStr)
	if err != nil {
		return err
	}
	acked[paramPrivateKey] = true

	opaqueStr, present := unparsed[paramOpaqueId]
	if !present {
		return ErrNoOpaqueId
	}
	opaqueId, err := crypting.LoadOpaqueIdText(opaqueStr)
	if err != nil {
		return err
	}
	acked[paramOpaqueId] = true

	spriv.LongtermPrivate = private
	spriv.OpaqueId = *opaqueId
	return nil
}

func ParseServerPrivate(unparsed map[string]string) (result *ServerPrivate, err error) {
	spriv := &ServerPrivate{EndpointParams: defEndpointParams}
	resultp, err := parseNew(parseable(spriv), unparsed)
	if resultp != nil {
		result = resultp.(*ServerPrivate)
	}
	return
}

func (spriv *ServerPrivate) UnparseInto(unparsed map[string]string) {
	unparsed[paramPrivateKey] = spriv.LongtermPrivate.PrivateText()
	unparsed[paramOpaqueId] = spriv.OpaqueId.Text()
	spriv.EndpointParams.UnparseInto(unparsed)
	return
}

func (spriv *ServerPrivate) Unparse() (unparsed map[string]string) {
	unparsed = make(map[string]string)
	spriv.UnparseInto(unparsed)
	return
}

const (
	horizontalWhitespace = " \t"
	keyBadChars          = "\r\n \t=#"
	valBadChars          = "\r\n"
)

// LoadServerPrivateFile loads server private identity information from path.
func LoadServerPrivateFile(path string) (result *ServerPrivate, err error) {
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
	unparsed := make(map[string]string)

	for lines.Scan() {
		line := lines.Text()
		if line[0] == '#' {
			continue
		}

		equals := strings.IndexRune(line, '=')
		if equals == -1 {
			return nil, ErrSyntax
		}

		key := strings.Trim(line[:equals], horizontalWhitespace)
		val := strings.Trim(line[equals+1:], horizontalWhitespace)
		unparsed[key] = val
	}

	if scanError := lines.Err(); scanError != nil {
		return nil, scanError
	}

	if closeError := file.Close(); closeError != nil {
		file = nil
		return nil, closeError
	}
	file = nil

	return ParseServerPrivate(unparsed)
}

const (
	friendlyHeader = "# This is a Dust private identity file."
)

func runeAt(s string, byteIndex int) rune {
	for _, r := range s[byteIndex:] {
		return r
	}

	return rune(0xfffd)
}

// SavePrivateFile saves the given server private identity information to a new file named path.  The file
// must not already exist.
func (spriv ServerPrivate) SavePrivateFile(path string) (err error) {
	unparsed := spriv.Unparse()
	lines := make([]string, 0, 2+len(unparsed))
	lines = append(lines, friendlyHeader)
	// TODO: put a timestamp or something in here?
	for key, val := range unparsed {
		if strings.IndexAny(key, keyBadChars) != -1 || strings.IndexAny(val, valBadChars) != -1 || strings.IndexRune(horizontalWhitespace, runeAt(val, 0)) != -1 {
			err = ErrSyntax
			return
		}
		lines = append(lines, key+"="+val)
	}
	lines = append(lines, "")
	content := strings.Join(lines, "\n")

	var file *os.File
	defer func() {
		if file != nil {
			if closeErr := file.Close(); closeErr != nil && err == nil {
				err = closeErr
			}

			if err != nil {
				_ = os.Remove(path)
			}
		}
	}()

	if file, err = os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0600); err != nil {
		return
	}

	if _, err = file.Write([]byte(content)); err != nil {
		return
	}

	if err = file.Sync(); err != nil {
		return
	}

	// Close happens in defer above.
	return nil
}
