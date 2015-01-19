package crypting

import (
	"bytes"
	"errors"
	"net"

	"github.com/blanu/Dust/go/Dust/cryptions"
)

type Public struct {
	IdBytes []byte
	LongtermPublic cryptions.PublicKey
}

type Private struct {
	IdBytes []byte
	LongtermPair cryptions.KeyPair
}

func (priv *Private) Destroy() {
	priv.LongtermPair.DestroyPrivate()
}

var (
	ErrUnrecognizedAddressType = errors.New("unrecognized address type")
	ipv4MappedPrefix = []byte{0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 0xff, 0xff}
)

func IdentityBytesOfNetworkAddress(netAddr interface{}) ([]byte, error) {
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
