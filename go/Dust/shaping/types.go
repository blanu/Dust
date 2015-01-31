/*
Package shaping implements the shaping layer of the Dust protocol suite.
*/
package shaping

import (
	"time"
)

// TODO: higher-performance buffer discipline for encode/decode?
// TODO: RecordPacketSent?

// Encoder applies to the sending side of a Dust half-connection.  It outputs timing and length parameters for
// outgoing packets, as well as stateful shaping of the byte stream.
type Encoder interface {
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

// Decoder applies to the receiving side of a Dust half-connection.  Incoming "soft" characteristics such as
// timing and length are discarded, but the decoder still provides the stateful inverse of the content shaping
// for the byte stream.
type Decoder interface {
	// UnshapeBytes takes the next chunk of shaped bytes and returns a decoded chunk of (expected-to-be)
	// uniform bytes, streaming-style.  The decoder may not retain p, but is presumed to keep other state
	// as necessary.
	UnshapeBytes(p []byte) []byte
}
