package Dust

import (
	"bytes"
	"errors"
	"net"
	"os"
	
	. "caolta3iaejox3z4madc5wmp4z.uuid/Dust4go/DustCrypto"
)

type CryptoServerIdentity struct {
	idBytes []byte
	longtermPublic PublicKey
}

type CryptoServerPrivate struct {
	idBytes []byte
	longtermPair KeyPair
}

func (spriv *CryptoServerPrivate) DestroyPrivate() {
	spriv.longtermPair.DestroyPrivate()
}

var (
	ErrUnrecognizedAddressType = errors.New("unrecognized address type")
	ErrMissingParameters = errors.New("missing parameters")
)

func cryptoIdBytes(netAddr interface{}) ([]byte, error) {
	var ip net.IP
	var port int
	var l3Flag uint8
	var l4Flag uint8
	
	switch a := netAddr.(type) {
	default:
		return nil, ErrUnrecognizedAddressType
	case *net.TCPAddr:
		ip, port = a.IP, a.Port
		l4Flag = 0x00
	}

	// INCOMPLETE: this is wrong, as sometimes net.IP stores 16-octet slices with IPv4-mapped format.  Ugh.
	switch len(ip) {
	default:
		return nil, ErrUnrecognizedAddressType
	case net.IPv4len:
		l3Flag = 0x00
	case net.IPv6len:
		l3Flag = 0x01
	}

	idBytes := bytes.Join([][]byte{
		[]byte{l3Flag | l4Flag}, ip,
		[]byte{uint8(port >> 8), uint8(port & 0xff)},
	}, []byte{})
	return idBytes, nil
}

const (
	bridgeParamPublicKey string = "p"
)

func LoadCryptoServerIdentityBridgeLine(
	address net.IP, port int,
	params map[string]string,
) (*CryptoServerIdentity, error) {
	publicString, found := params[bridgeParamPublicKey]
	if !found {
		return nil, ErrMissingParameters
	}

	idBytes, err := cryptoIdBytes(&net.TCPAddr{IP: address, Port: port})
	if err != nil {
		return nil, err
	}
	longtermPublic, err := LoadPublicKeyBase32(publicString)
	if err != nil {
		return nil, err
	}
	return &CryptoServerIdentity{idBytes, longtermPublic}, nil
}

func LoadCryptoServerPrivateFile(
	address net.IP, port int,
	path string,
) (*CryptoServerPrivate, error) {
	var file *os.File
	var err error
	defer func() {
		if file != nil {
			_ = file.Close()
		}
	}()
	
	if file, err = os.Open(path); err != nil {
		return nil, err
	}

	var privateRepr SecretBytes
	ok := false
	defer func() {
		if !ok && privateRepr != nil {
			privateRepr.Destroy()
		}
	}()
	privateRepr = NewSecretBytes()
	
	n, err := file.Read(privateRepr.Slice())
	if err != nil {
		return nil, err
	}
	if n != 32 {
		return nil, ErrBadPrivateKey
	}
	_ = file.Close()
	file = nil
	
	idBytes, err := cryptoIdBytes(&net.TCPAddr{IP: address, Port: port})
	if err != nil {
		return nil, err
	}
	
	longtermPair, err := LoadKeyPairOwningPrivate(privateRepr)
	privateRepr = nil
	if err != nil {
		return nil, err
	}

	spriv := &CryptoServerPrivate{idBytes, longtermPair}
	ok = true
	return spriv, nil
}

func NewCryptoServerPrivate(address net.IP, port int) (*CryptoServerPrivate, error) {
	idBytes, err := cryptoIdBytes(&net.TCPAddr{IP: address, Port: port})
	if err != nil {
		return nil, err
	}
	
	ok := false
	var longtermPair KeyPair
	defer func() {
		if !ok && longtermPair != nil {
			longtermPair.DestroyPrivate()
		}
	}()
	if longtermPair, err = NewKeyPair(); err != nil {
		return nil, err
	}

	spriv := &CryptoServerPrivate{idBytes, longtermPair}
	ok = true
	return spriv, nil
}

func (spriv *CryptoServerPrivate) SavePrivateFile(path string) error {
	var file *os.File
	var err error
	defer func() {
		if file != nil {
			file.Close()
		}
	}()
	
	if file, err = os.OpenFile(path, os.O_CREATE | os.O_EXCL | os.O_WRONLY, 0600); err != nil {
		return err
	}

	if _, err = file.Write(spriv.longtermPair.PrivateSlice()); err != nil {
		_ = file.Close()
		file = nil
		_ = os.Remove(path)
		return err
	}

	err = file.Close()
	file = nil
	if err != nil {
		return err
	}

	return nil
}

func (spriv *CryptoServerPrivate) BridgeParams() map[string]string {
	return map[string]string{
		bridgeParamPublicKey: spriv.longtermPair.Public().Base32(),
	}
}
