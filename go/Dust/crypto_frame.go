package Dust

import (
	"crypto/cipher"
	. "github.com/blanu/Dust/go/DustCrypto"
)

// A cryptoFrame represents a plaintext frame at the Dust crypto session layer as a slice of bytes.  Except
// occasionally it represents an encrypted frame, because that API should be changed.  Oops.
type cryptoFrame []byte

// TODO: rename to newDataFrame?

// newPlainDataFrame constructs a cryptoFrame containing the given data and an invalid authenticator.
func newPlainDataFrame(data []byte) cryptoFrame {
	if len(data) > 65535 {
		panic("data too long for frame representation")
	}
	
	payloadSize := len(data) + 33
	frame := make([]byte, 2 + payloadSize)
	frame[0] = uint8(payloadSize >> 8)
	frame[1] = uint8(payloadSize & 0xff)
	frame[2] = 0x01
	copy(frame[3:], data)
	// Leave space for the authenticator, but don't actually compute it yet.
	return cryptoFrame(frame)
}

// newPaddingFrame constructs a padding cryptoFrame whose wire size should be as close to requestedSize as
// possible.  It is guaranteed that the resultant frame will be at least one octet in length, but the size
// is otherwise not guaranteed.
func newPaddingFrame(requestedSize int) cryptoFrame {
	size := requestedSize
	if size < 35 {
		size = 35
	}
	if size > 65535 {
		size = 65535
	}

	frame := make([]byte, size)
	payloadSize := len(frame) - 2
	frame[0] = uint8(payloadSize >> 8)
	frame[1] = uint8(payloadSize & 0xff)
	frame[2] = 0x00
	return cryptoFrame(frame)
}

// incomingCryptoFrame takes a chunk of incoming plaintext octet stream which must start at a frame boundary.
// It returns a cryptoFrame corresponding to (and possibly aliasing) the first decodable frame (regardless of
// authentication or data), or nil if no complete frame is present at the beginning of the chunk.
func incomingCryptoFrame(p []byte) cryptoFrame {
	var wireSize int
	if len(p) >= 2 {
		wireSize = 2 + int(uint16(p[0]) << 8 | uint16(p[1]))
	} else {
		wireSize = 0
	}

	if !(0 < wireSize && wireSize <= len(p)) {
		return nil
	}
	return cryptoFrame(p[:wireSize])
}

// wellFormed returns true iff the frame is decode-valid, ignoring data and authenticator.
func (frame cryptoFrame) wellFormed() bool {
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
func (frame cryptoFrame) authenticateWith(authenticatorKey SecretBytes) {
	authenticator := ComputeAuthenticator(frame[:len(frame)-32], authenticatorKey)
	copy(frame[len(frame)-32:], authenticator)
}

// verifyAuthenticator returns true iff the frame's authenticator is valid based on authenticatorKey.
func (frame cryptoFrame) verifyAuthenticator(authenticatorKey SecretBytes) bool {
	return VerifyAuthenticator(frame[:len(frame)-32], authenticatorKey, frame[len(frame)-32:])
}

// hasData returns true iff the frame contains data that should be passed through to the next layer.
func (frame cryptoFrame) hasData() bool {
	return frame[2] & 1 != 0
}

// data returns a slice with the application data to pass through, which may only be valid if hasData
// returned true.
func (frame cryptoFrame) data() []byte {
	return frame[3:len(frame)-32]
}

// encryptWith applies streamCipher to the frame in-place.
func (frame cryptoFrame) encryptWith(streamCipher cipher.Stream) {
	// TODO: use a different API for this so cryptoFrame stays plaintext frames only
	streamCipher.XORKeyStream(frame, frame)
}

// slice returns a slice which may or may not alias the frame contents.
func (frame cryptoFrame) slice() []byte {
	return frame
}

// wireSize returns the total number of octets the frame takes on the wire.
func (frame cryptoFrame) wireSize() int {
	return len(frame)
}

