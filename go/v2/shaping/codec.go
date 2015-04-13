// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

/*
Package shaping implements the shaping layer of the Dust protocol suite.
*/
package shaping

import (
	"time"
)

// Encoder applies to the sending side of a Dust half-connection.  It outputs timing and length parameters for
// outgoing packets, as well as stateful shaping of the byte stream.
type Encoder interface {
	// MaxPacketLength returns any uint16 greater than or equal to any length that will ever be returned
	// by NextPacketLength from this encoder.
	MaxPacketLength() uint16

	// NextPacketLength returns the length of the next packet that should be sent.  It and NextPacketSleep
	// may be called in either order.
	NextPacketLength() uint16

	// NextPacketSleep returns a duration to wait before sending the next packet.  It and NextPacketLength
	// may be called in either order.
	NextPacketSleep() time.Duration

	// ShapeBytes continues transforming a uniform byte stream into a shaped byte stream.  Upon return, it
	// has consumed sn bytes of src and overwritten dn bytes of dst, and len(src) == sn or len(dst) == dn.
	ShapeBytes(dst, src []byte) (dn, sn int)
}

// Decoder applies to the receiving side of a Dust half-connection.  Incoming "soft" characteristics such as
// timing and length are discarded, but the decoder still provides the stateful inverse of the content shaping
// for the byte stream.
type Decoder interface {
	// UnshapeBytes continues transforming a shaped byte stream into an expected-to-be-uniform byte stream.
	// Upon return, it has consumed sn bytes of src and overwritten dn bytes of dst, and len(src) == sn or
	// len(dst) == dn.
	UnshapeBytes(dst, src []byte) (dn, sn int)
}
