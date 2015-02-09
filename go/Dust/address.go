/*
Package Dust implements the main Dust protocol suite for TCP/IP.

At initialization time, use RegisterModel to associate model names with model constructors.  Attempting to
register the same model name multiple times will panic.

To connect to a Dust server, use LoadServerPublicBridgeLine to load its public server identity.  To act as
a Dust server, use LoadServerPrivateFile to load your private server identity.  Then, use one of the following
modes.  (TODO: Or, you _would_ be able to use one of the following modes, if they all existed.)

Raw mode: as either client or server, you must provide suitable sockets to use directly (currently, TCP
connections).  Use BeginRawClient or BeginRawServer; the Dust code will take over the socket.  You can then
read and write datagrams until the model in use deems that the underlying connection should be closed.  You
probably don't want this mode for most applications.

Message mode: you must provide a way of dialing or accepting suitable sockets.  After BeginMessageClient or
BeginMessageServer, you can read and write datagrams across an entire group of underlying connections.  For
the client, read datagrams can be associated with each other and with previously written datagrams, but you
cannot control the association of datagrams that you write.  For the server, read datagrams can be associated
with each other, but written datagrams must be addressed to a specific client.

Stream mode: you must provide a way of dialing or accepting suitable sockets.  After BeginStreamClient or
BeginStreamServer, you can dial or accept streams across an entire group of underlying connections.  Data
within a stream is associated regardless of the underlying Dust connections it is transceived on.
*/
package Dust

import (
	"errors"
	"fmt"
	"sync/atomic"
)

var (
	// TODO: use some error from net?
	ErrAfNoSupport = errors.New("Dust: address family not supported")
	ErrUnreachable = errors.New("Dust: address not reachable")
)

type addrToken struct {
	ptr *uint64
}

var (
	nextAddrTokenStorage uint64 = 0
)

func mungeAddrTokenValue(n uint64) uint64 {
	// Might want to put some cosmetic munging in here later.
	return n
}

func nextAddrToken() addrToken {
	ptr := new(uint64)
	*ptr = atomic.AddUint64(&nextAddrTokenStorage, uint64(1))
	return addrToken{ptr}
}

func (token addrToken) String() string {
	if token.ptr == nil {
		return "ANY"
	}

	n := mungeAddrTokenValue(*token.ptr)
	w, x, y, z := uint16(n>>48), uint16(n>>32), uint16(n>>16), uint16(n)
	if w == 0 && x == 0 {
		return fmt.Sprintf("%x.%x", y, z)
	} else {
		return fmt.Sprintf("%x.%x.%x.%x", w, x, y, z)
	}
}

const (
	linkNetwork = "Dust_link"
)

type LinkAddr addrToken

func (addr LinkAddr) Network() string {
	return linkNetwork
}

func (addr LinkAddr) String() string {
	return fmt.Sprintf("%s:%s", linkNetwork, addrToken(addr).String())
}

func (addr LinkAddr) IsZero() bool {
	return addrToken(addr).ptr == nil
}

func LinkAddrEqual(a, b LinkAddr) bool {
	return a.ptr == b.ptr
}

func genLinkAddr() LinkAddr {
	return LinkAddr(nextAddrToken())
}
