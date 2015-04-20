// Copyright © 2012 Dmitry Chestnykh, 2015 Drake Wilson.  All rights reserved.  Use of this source code is
// governed by a BSD-style license that can be found in the ../LICENSE.md file.

// Package skein implements Skein-512 as defined in "The Skein Hash Function Family, v1.3".  This version of
// the skein package is somewhat revamped for use in the Dust library, changing the API to remove some functions
// and expose the initial-value chaining.
package skein

import (
	"errors"

	"github.com/blanu/Dust/go/buf"
)

var (
	ErrWrongState = errors.New("skein: running digest in wrong state")
)

const (
	schemaAndVersion uint64 = 0x0000000133414853
)

type Initial struct {
	chain block
	next  Arg
	osize uint64
}

func (ini *Initial) addConfig() {
	var tw tweak = blockTweak(32, ArgConfig)

	var input block
	input[0] = schemaAndVersion
	switch ini.osize {
	case ^uint64(0):
		input[1] = ^uint64(0)
	default:
		input[1] = ini.osize * 8
	}

	ini.chain.input(&tw, &input)
}

// AddArg advances the state of ini to include val as the value for arg if val is not nil.  If val is nil,
// AddArg does nothing.  If val is not nil, arg must be greater than any previous value of arg.
func (ini *Initial) AddArg(arg Arg, val []byte) {
	if val == nil {
		return
	}

	switch {
	case arg < ini.next:
		panic("skein: bad argument sequencing")
	case ini.next <= ArgConfig && ArgConfig < arg:
		ini.addConfig()
	}

	var tw tweak = firstTweak(arg)
	var input block
	for len(val) > 0 {
		tw[0] += uint64(input.consume(&val))
		if len(val) == 0 {
			tw[1] |= flagLast
		}
		ini.chain.input(&tw, &input)
		tw[1] &^= flagFirst
	}

	ini.next = arg+1
}

func (ini *Initial) Finish() {
	if ini.next <= ArgConfig {
		ini.addConfig()
		ini.next = ArgConfig+1
	}
}

func (ini *Initial) Init(osize uint64, args *Args) {
	*ini = Initial{
		next:  Arg(0),
		osize: osize,
	}

	if args != nil {
		ini.AddArg(ArgKey, args.Key)
		ini.AddArg(ArgPerson, args.Person)
		ini.AddArg(ArgPublic, args.PublicKey)
		ini.AddArg(ArgKeyId, args.KeyId)
		ini.AddArg(ArgNonce, args.Nonce)
	}

	ini.Finish()
}

func (ini *Initial) Chain() (out [8]uint64) {
	copy(out[:], ini.chain[:])
	return
}

type state uint8

const (
	stateInput state = iota
	stateOutput
)

type Hash struct {
	state state
	chain block
	tweak tweak
	osize uint64

	xfer buf.Reassembly
	xsto [blockSizeBytes]byte
	octr uint64
}

func (h *Hash) Copy() (h1 Hash) {
	h1 = *h
	h1.xfer = buf.ExistingReassemblyArray(h1.xsto[:], h.xfer.ValidLen())
	return
}

func (h *Hash) processBlock(b []byte) {
	var input block
	h.tweak[0] += uint64(input.consume(&b))
	h.chain.input(&h.tweak, &input)
	h.tweak[1] &^= flagFirst
}

func (h *Hash) finished() bool {
	return h.tweak.arg() == ArgOutput
}

func (h *Hash) Write(p []byte) (n int, err error) {
	if h.state != stateInput {
		return 0, ErrWrongState
	}

	n += buf.CopyReassemble(&h.xfer, &p)
	switch {
	case !h.xfer.FixedSizeComplete():
		// Less than a single block available to process.
		return
	case len(p) == 0:
		// We can't process the block yet, because it might or might not be the last one.
		return
	}

	// Definitely not the last block.
	h.processBlock(h.xfer[:])
	h.xfer.Reset()

	// This must be greater-than, not greater-or-equal, because we only want to process
	// non-final blocks here.
	for len(p) > blockSizeBytes {
		h.processBlock(p[:blockSizeBytes])
		p = p[blockSizeBytes:]
		n += blockSizeBytes
	}

	n += buf.CopyReassemble(&h.xfer, &p)
	return
}

func (h *Hash) CloseWrite() (err error) {
	if h.state == stateOutput {
		return
	}

	// We always need to process at least one block, so this happens even if h.xfer is empty.
	h.tweak[1] |= flagLast
	h.processBlock(h.xfer[:])

	h.state = stateOutput
	h.octr = 0
	h.xfer.Reset()
	return
}

func (h *Hash) nextOutput() {
	var output block
	h.chain.output(h.octr, &output)
	h.octr++

	dst := h.xfer.PreData(blockSizeBytes)
	for i, v := range output[:] {
		dst[i*8+0] = byte(v)
		dst[i*8+1] = byte(v >> 8)
		dst[i*8+2] = byte(v >> 16)
		dst[i*8+3] = byte(v >> 24)
		dst[i*8+4] = byte(v >> 32)
		dst[i*8+5] = byte(v >> 40)
		dst[i*8+6] = byte(v >> 48)
		dst[i*8+7] = byte(v >> 56)
	}
}

func (h *Hash) Read(p []byte) (n int, err error) {
	_ = h.CloseWrite()

	for {
		n += h.xfer.CopyOut(&p)
		if len(p) == 0 {
			break
		}
		h.nextOutput()
	}

	return
}

func (h *Hash) XORKeyStream(dst, src []byte) {
	_ = h.CloseWrite()

	for len(dst) > 0 {
		if h.xfer.Empty() {
			h.nextOutput()
		}

		subn := len(dst)
		if h.xfer.ValidLen() < subn {
			subn = h.xfer.ValidLen()
		}

		for i := 0; i < subn; i++ {
			dst[i] = src[i] ^ h.xfer[i]
		}
		h.xfer.Consume(subn)
		dst = dst[subn:]
		src = src[subn:]
	}
}

func (h *Hash) InitFrom(ini *Initial) {
	*h = Hash{
		state: stateInput,
		chain: ini.chain,
		tweak: firstTweak(ArgMessage),
		osize: ini.osize,
	}

	h.xfer = buf.BeginReassemblyArray(h.xsto[:])
}

func (h *Hash) Init(osize uint64, args *Args) {
	var ini Initial
	ini.Init(osize, args)
	h.InitFrom(&ini)
}
