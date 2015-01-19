package Dust

import (
	"time"
)

// TODO: rename these to ShapingEncoder/ShapingDecoder, probably
// TODO: higher-performance buffer discipline for encode/decode?
// TODO: RecordPacketSent?

type ShapingEncoder interface {
	// WholeStreamDuration returns the length of time the connection should be kept open.
	WholeStreamDuration() time.Duration

	// MaxPacketLength returns any uint16 greater than or equal to any length that will ever be returned
	// by NextPacketLength from this encoder.
	MaxPacketLength() uint16

	// NextPacketLength returns the length of the next packet that should be sent.  It and NextPacketSleep
	// may be called in either order.
	NextPacketLength() uint16

	// NextPacketSleep returns a duration to wait before sending the next packet.  It and NextPacketLength
	// may be called in either order.
	NextPacketSleep() time.Duration

	// ShapeBytes takes the next chunk of uniform bytes and returns as many shaped bytes as it can at a
	// time, streaming-style.  The encoder may not retain p, but is presumed to keep other state as
	// necessary.
	ShapeBytes(p []byte) []byte
}

type ShapingDecoder interface {
	// UnshapeBytes takes the next chunk of shaped bytes and returns a decoded chunk of (expected-to-be)
	// uniform bytes, streaming-style.  The decoder may not retain p, but is presumed to keep other state
	// as necessary.
	UnshapeBytes(p []byte) []byte
}

type ShapingCodec interface {
	ShapingEncoder
	ShapingDecoder
}

type ModelConstructor func(map[string]string params) (ShapingEncoder, ShapingDecoder, error)
