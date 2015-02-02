/*
Package crypting implements the cryptographic and framing layer of the Dust protocol suite.
*/
package crypting

import (
	"bytes"
	"errors"
	"net"

	"github.com/blanu/Dust/go/Dust/cryptions"
)

// Public holds the longterm public key and shared identifier for a server.
type Public struct {
	IdBytes        []byte
	LongtermPublic cryptions.PublicKey
}

// Private holds the longterm private key and shared identifier for a server.
type Private struct {
	IdBytes      []byte
	LongtermPair cryptions.KeyPair
}

func (priv *Private) Destroy() {
	priv.LongtermPair.DestroyPrivate()
}

var (
	ErrUnrecognizedAddressType = errors.New("Dust: unrecognized address type")

	ipv4MappedPrefix = []byte{0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 0xff, 0xff}
)

// IdentityBytesOfNetworkAddress takes a network address and returns a shared identifier for it.  Currently,
// this address must be a TCP/IP address.  If the address is not of a type for which a shared identifier can
// be determined, ErrUnrecognizedAddressType is returned.
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
		if bytes.HasPrefix(ip, ipv4MappedPrefix) {
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
