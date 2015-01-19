package crypting

import (
	"crypto/cipher"

	"github.com/blanu/Dust/go/Dust/cryptions"
)

// A frame represents a plaintext frame at the Dust crypto session layer as a slice of bytes.  Except
// occasionally it represents an encrypted frame, because that API should be changed.  Oops.
type frame []byte

// TODO: rename to newDataFrame?

// newPlainDataFrame constructs a frame containing the given data and an invalid authenticator.
func newPlainDataFrame(data []byte) frame {
	if len(data) > 65535 {
		panic("data too long for frame representation")
	}
	
	payloadSize := len(data) + 33
	wire := make([]byte, 2 + payloadSize)
	wire[0] = uint8(payloadSize >> 8)
	wire[1] = uint8(payloadSize & 0xff)
	wire[2] = 0x01
	copy(wire[3:], data)
	// Leave space for the authenticator, but don't actually compute it yet.
	return frame(wire)
}

// newPaddingFrame constructs a padding frame whose wire size should be as close to requestedSize as
// possible.  It is guaranteed that the resultant frame will be at least one octet in length, but the size
// is otherwise not guaranteed.
func newPaddingFrame(requestedSize int) frame {
	size := requestedSize
	if size < 35 {
		size = 35
	}
	if size > 65535 {
		size = 65535
	}

	wire := make([]byte, size)
	payloadSize := len(wire) - 2
	wire[0] = uint8(payloadSize >> 8)
	wire[1] = uint8(payloadSize & 0xff)
	wire[2] = 0x00
	return frame(wire)
}

// incomingCryptoFrame takes a chunk of incoming plaintext octet stream which must start at a frame boundary.
// It returns a frame corresponding to (and possibly aliasing) the first decodable frame (regardless of
// authentication or data), or nil if no complete frame is present at the beginning of the chunk.
func incomingCryptoFrame(p []byte) frame {
	var wireSize int
	if len(p) >= 2 {
		wireSize = 2 + int(uint16(p[0]) << 8 | uint16(p[1]))
	} else {
		wireSize = 0
	}

	if !(0 < wireSize && wireSize <= len(p)) {
		return nil
	}
	return frame(p[:wireSize])
}

// wellFormed returns true iff the frame is decode-valid, ignoring data and authenticator.
func (frame frame) wellFormed() bool {
	switch {
	case len(frame) < 35:
		return false
	case len(frame) != 2 + int(uint16(frame[0]) << 8 | uint16(frame[1])):
		return false
	default:
		return true
	}
}

// authenticateWith mutates a frame to contain a valid authenticator based on authenticatorKey.
func (frame frame) authenticateWith(authenticatorKey cryptions.SecretBytes) {
	authenticator := cryptions.ComputeAuthenticator(frame[:len(frame)-32], authenticatorKey)
	copy(frame[len(frame)-32:], authenticator)
}

// verifyAuthenticator returns true iff the frame's authenticator is valid based on authenticatorKey.
func (frame frame) verifyAuthenticator(authenticatorKey cryptions.SecretBytes) bool {
	return cryptions.VerifyAuthenticator(frame[:len(frame)-32], authenticatorKey, frame[len(frame)-32:])
}

// hasData returns true iff the frame contains data that should be passed through to the next layer.
func (frame frame) hasData() bool {
	return frame[2] & 1 != 0
}

// data returns a slice with the application data to pass through, which may only be valid if hasData
// returned true.
func (frame frame) data() []byte {
	return frame[3:len(frame)-32]
}

// encryptWith applies streamCipher to the frame in-place.
func (frame frame) encryptWith(streamCipher cipher.Stream) {
	// TODO: use a different API for this so frame stays plaintext frames only
	streamCipher.XORKeyStream(frame, frame)
}

// slice returns a slice which may or may not alias the frame contents.
func (frame frame) slice() []byte {
	return frame
}

// wireSize returns the total number of octets the frame takes on the wire.
func (frame frame) wireSize() int {
	return len(frame)
}

