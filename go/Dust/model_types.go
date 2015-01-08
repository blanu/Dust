package Dust

import (
	"time"
)

// INCOMPLETE: there is no actual shaping model here yet.

// TODO: higher-performance buffer discipline for encode/decode?

type EncodeModel interface {
	WholeStreamDuration() time.Duration
	MaxPacketLength() uint16
	
	NextPacketLength() uint16
	NextPacketSleep() time.Duration
	EncodeBytes(p []byte) []byte
}

type DecodeModel interface {
	DecodeBytes(p []byte) []byte
}

type Model interface {
	EncodeModel
	DecodeModel
}
