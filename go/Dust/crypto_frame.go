package Dust

import (
	"crypto/cipher"
	. "github.com/blanu/Dust/go/DustCrypto"
)

type cryptoFrame []byte

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

func (frame cryptoFrame) authenticateWith(authenticatorKey SecretBytes) {
	authenticator := ComputeAuthenticator(frame[:len(frame)-32], authenticatorKey)
	copy(frame[len(frame)-32:], authenticator)
}

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

func (frame cryptoFrame) verifyAuthenticator(authenticatorKey SecretBytes) bool {
	return VerifyAuthenticator(frame[:len(frame)-32], authenticatorKey, frame[len(frame)-32:])
}

func (frame cryptoFrame) hasData() bool {
	return frame[2] & 1 != 0
}

func (frame cryptoFrame) data() []byte {
	return frame[3:len(frame)-32]
}

func (frame cryptoFrame) encryptWith(streamCipher cipher.Stream) {
	streamCipher.XORKeyStream(frame, frame)
}

func (frame cryptoFrame) slice() []byte {
	return frame
}

func (frame cryptoFrame) wireSize() int {
	return len(frame)
}

