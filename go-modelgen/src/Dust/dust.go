package Dust

type Dust interface {
	Duration() uint16
	PacketCount(milliseconds uint16) uint16
	PacketLength() uint16
	Encode(bytes []byte) []byte
	Decode(bytes []byte) []byte
	RandomBytes(len uint16) []byte
}

//go:generate python compile-go.py
