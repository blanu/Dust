// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

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
