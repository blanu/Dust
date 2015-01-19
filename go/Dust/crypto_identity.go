package Dust

import (
	"bytes"
	"errors"
	"net"
	
	. "github.com/blanu/Dust/go/DustCrypto"
)

// TODO: continue restructuring these along with highlevel_api.go stuff

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
