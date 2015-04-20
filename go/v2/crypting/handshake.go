// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package crypting

import (
	"github.com/blanu/Dust/go/prim1"
)

const (
	kdfC2S = `cli.`
	kdfS2C = `srv.`

	kdfCipherData = `dta.`
	kdfMACData    = `grm.`
	kdfMACConfirm = `bgn.`
)

type streamKeys struct {
	cipherKey prim.CipherKey
	authKey   prim.AuthKey
	confirm   prim.AuthValue
}

func (sk *streamKeys) Clear() {
	sk.cipherKey = prim.ZeroCipherKey()
	sk.authKey = prim.ZeroAuthKey()
	sk.confirm = prim.ZeroAuthValue()
}

type handshake struct {
	local  prim.Private
	remote prim.Public
	sinfo  interface{}

	in, out streamKeys
}

func (hs *handshake) start(sinfo interface{}) []byte {
	hs.local = prim.NewPrivate()
	hs.sinfo = sinfo
	return hs.local.Public.Binary()
}

func (hs *handshake) completeWith(received []byte) {
	var err error
	if len(received) != prim.PublicBinaryLen {
		panic("Dust/crypting: should not have gotten here without PublicBinaryLen bytes")
	}

	hs.remote, err = prim.LoadPublicBinary(received)
	if err != nil {
		panic("Dust/crypting: should always be able to load public key here")
	}

	var ntor prim.NtorHandshake
	var inKdfPrefix, outKdfPrefix string

	switch sinfo := hs.sinfo.(type) {
	default:
		panic("Dust/crypting: bad serverInfo type")

	case *Public:
		// We're the client.
		inKdfPrefix, outKdfPrefix = kdfS2C, kdfC2S
		ntor.Init(sinfo.Id[:], &hs.local.Public, &hs.remote)
		ntor.WriteDHPart(hs.local.SharedSecret(hs.remote))
		ntor.WriteDHPart(hs.local.SharedSecret(sinfo.Key))

	case *Private:
		// We're the server.
		inKdfPrefix, outKdfPrefix = kdfC2S, kdfS2C
		ntor.Init(sinfo.Id[:], &hs.remote, &hs.local.Public)
		ntor.WriteDHPart(hs.local.SharedSecret(hs.remote))
		ntor.WriteDHPart(sinfo.Key.SharedSecret(hs.remote))
	}

	var secret prim.Secret
	secret, hs.in.confirm, hs.out.confirm = ntor.Finish(inKdfPrefix, outKdfPrefix)
	hs.in.cipherKey = secret.DeriveCipherKey(inKdfPrefix + kdfCipherData)
	hs.in.authKey = secret.DeriveAuthKey(inKdfPrefix + kdfMACData)
	hs.out.cipherKey = secret.DeriveCipherKey(outKdfPrefix + kdfCipherData)
	hs.out.authKey = secret.DeriveAuthKey(outKdfPrefix + kdfMACData)
}
