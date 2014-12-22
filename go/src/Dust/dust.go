package Dust

type Dust interface {
  duration() uint16
  packet_count(milliseconds uint16) uint16
  packet_length() uint16
  encode(bytes []byte) []byte
  add_padding(bytes []byte, length uint16) []byte
  random_bytes(len uint16) []byte
  decode(bytes []byte) []byte
  strip_padding(bytes []byte) []byte
}

//go:generate python compile-go.py
