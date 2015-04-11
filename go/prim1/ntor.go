// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../LICENSE.md.

package prim

import (
	"bytes"

	"github.com/blanu/Dust/go/skein"
)

const (
	personNtorH = personPrefix + "ntor-H.skein"
	personNtorHmac = personPrefix + "ntor-Hmac.skein"

	ntorSquawk = `ntor(Dust2015)`
	ntorServer = `server`
	ntorClient = `client`
)

type NtorHandshake struct {
	h skein.Hash

	x, y []byte
	bHat []byte
}

func makeInitial(osize uint64, args *skein.Args) (out skein.Initial) {
	out.Init(osize, args)
	return
}

var (
	ntorInitialH    = makeInitial(CValueLen + SecretLen, &skein.Args{Person: []byte(personNtorH)})
	ntorInitialHmac = makeInitial(AuthLen, &skein.Args{Person: []byte(personNtorHmac)})
)

func (ntor *NtorHandshake) Init(bHat []byte, x, y *Public) {
	ntor.h.InitFrom(&ntorInitialH)
	ntor.x = x.Binary()
	ntor.y = y.Binary()
	ntor.bHat = bHat
}

func (ntor *NtorHandshake) WriteDHPart(secret Secret) {
	ntor.h.Write(secret[:])
}

func (ntor *NtorHandshake) Finish(inTail, outTail string) (shared Secret, tIn, tOut AuthValue) {
	// The DH parts have already been written into ntor.h.
	hTail := bytes.Join([][]byte{ntor.bHat, ntor.x, ntor.y, []byte(ntorSquawk)}, nil)
	ntor.h.Write(hTail)

	var confSeed CValue
	_, _ = ntor.h.Read(confSeed[:])
	_, _ = ntor.h.Read(shared[:])

	hMacMedial := bytes.Join([][]byte{ntor.bHat, ntor.y, ntor.x, []byte(ntorSquawk)}, nil)
	var hMac skein.Hash
	hMac.InitFrom(&ntorInitialHmac)
	hMac.Write(confSeed[:])
	hMac.Write(hMacMedial)

	hMacIn := hMac.Copy()
	hMacIn.Write([]byte(inTail))
	_, _ = hMacIn.Read(tIn[:])
	hMacOut := hMac.Copy()
	hMacOut.Write([]byte(outTail))
	_, _ = hMacOut.Read(tOut[:])

	return
}

