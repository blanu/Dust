package Dust

import (
	"bytes"
	"errors"
	"net"
	"os"
	
	. "github.com/blanu/Dust/go/DustCrypto"
)

// TODO: rename to CryptoServerPublic?  Maybe bundle models in here and make DustServerIdentity... ?

// TODO: make address space generic in terms of exported type signatures, even though all that is handled
// in this library version is TCP addresses

type CryptoServerIdentity struct {
	tcpAddr *net.TCPAddr
	idBytes []byte
	longtermPublic PublicKey
}

func (sid *CryptoServerIdentity) DialAddr() *net.TCPAddr {
	return sid.tcpAddr
}

type CryptoServerPrivate struct {
	tcpAddr *net.TCPAddr
	idBytes []byte
	longtermPair KeyPair
}

func (spriv *CryptoServerPrivate) ListenAddr() *net.TCPAddr {
	return spriv.tcpAddr
}

func (spriv *CryptoServerPrivate) DestroyPrivate() {
	spriv.longtermPair.DestroyPrivate()
}

var (
	ErrUnrecognizedAddressType = errors.New("unrecognized address type")
	ErrMissingParameters = errors.New("missing parameters")
	ErrFileTooShort = errors.New("file too short")
	ipv4MappedPrefix = []byte{0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 0xff, 0xff}
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

	switch len(ip) {
	default:
		return nil, ErrUnrecognizedAddressType
	case net.IPv4len:
		l3Flag = 0x00
	case net.IPv6len:
		// Sometimes net.IP stores IPv4 addresses in 16-byte slices too.  Sigh.
		if bytes.Equal(ip[:len(ipv4MappedPrefix)], ipv4MappedPrefix) {
			l3Flag = 0x00
		} else {
			l3Flag = 0x01
		}
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
	addrString string,
	params map[string]string,
) (*CryptoServerIdentity, error) {
	// TODO: probably don't allow resolving here.  Use explicit host/port split and IP address parsing
	// instead.
	tcpAddr, err := net.ResolveTCPAddr("tcp", addrString)
	if err != nil {
		return nil, err
	}
	
	publicString, found := params[bridgeParamPublicKey]
	if !found {
		return nil, ErrMissingParameters
	}

	idBytes, err := cryptoIdBytes(tcpAddr)
	if err != nil {
		return nil, err
	}
	longtermPublic, err := LoadPublicKeyBase32(publicString)
	if err != nil {
		return nil, err
	}
	return &CryptoServerIdentity{tcpAddr, idBytes, longtermPublic}, nil
}

func LoadCryptoServerPrivateFile(
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

	var n int
	addrRepr := make([]byte, 18)
	n, err = file.Read(addrRepr)
	if err != nil {
		return nil, err
	}
	if n != len(addrRepr) {
		return nil, ErrFileTooShort
	}

	ip := net.IP(addrRepr[0:16])
	port := int(uint16(addrRepr[16]) << 8 | uint16(addrRepr[17]))
	tcpAddr := &net.TCPAddr{IP: ip, Port: port}

	var privateRepr SecretBytes
	ok := false
	defer func() {
		if !ok && privateRepr != nil {
			privateRepr.Destroy()
		}
	}()
	privateRepr = NewSecretBytes()
	
	n, err = file.Read(privateRepr.Slice())
	if err != nil {
		return nil, err
	}
	if n != 32 {
		return nil, ErrFileTooShort
	}
	_ = file.Close()
	file = nil
	
	idBytes, err := cryptoIdBytes(tcpAddr)
	if err != nil {
		return nil, err
	}
	
	longtermPair, err := LoadKeyPairOwningPrivate(privateRepr)
	privateRepr = nil
	if err != nil {
		return nil, err
	}

	spriv := &CryptoServerPrivate{tcpAddr, idBytes, longtermPair}
	ok = true
	return spriv, nil
}

func NewCryptoServerPrivate(addrString string) (*CryptoServerPrivate, error) {
	tcpAddr, err := net.ResolveTCPAddr("tcp", addrString)
	if err != nil {
		return nil, err
	}
	
	idBytes, err := cryptoIdBytes(tcpAddr)
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

	spriv := &CryptoServerPrivate{tcpAddr, idBytes, longtermPair}
	ok = true
	return spriv, nil
}

func (spriv *CryptoServerPrivate) SavePrivateFile(path string) error {
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
	
	addrRepr := make([]byte, 18)
	copy(addrRepr[0:16], spriv.tcpAddr.IP.To16())
	port := spriv.tcpAddr.Port
	addrRepr[16] = uint8(port >> 8)
	addrRepr[17] = uint8(port & 0xff)
	
	if file, err = os.OpenFile(path, os.O_CREATE | os.O_EXCL | os.O_WRONLY, 0600); err != nil {
		return err
	}
	if _, err = file.Write(addrRepr); err != nil {
		return err
	}
	if _, err = file.Write(spriv.longtermPair.PrivateSlice()); err != nil {
		return err
	}

	err = file.Close()
	file = nil
	if err != nil {
		return err
	}

	commit = true
	return nil
}

func (spriv *CryptoServerPrivate) BridgeParams() map[string]string {
	return map[string]string{
		bridgeParamPublicKey: spriv.longtermPair.Public().Base32(),
	}
}
