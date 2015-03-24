package crypting

import (
	"bytes"

	"github.com/blanu/Dust/go/Dust/buf"
	"github.com/blanu/Dust/go/Dust/prim"
)

func (cs *Session) receivedEphemeralKey() {
	var err error

	received := cs.inHandshake.Data()
	if len(received) != 32 {
		panic("Dust/crypting: should not have gotten here without 32-byte handshake")
	}

	cs.remotePublic, err = prim.LoadPublicBinary(received)
	if err != nil {
		panic("Dust/crypting: should always be able to load public key here")
	}

	log.Debug("  <- received ephemeral key")

	var sd prim.SecretDigest
	sd.Init()
	sd.WriteSecret(cs.localPrivate.SharedSecret(cs.remotePublic))
	var inKdfPrefix, outKdfPrefix string
	var confInput []byte

	switch sinfo := cs.serverInfo.(type) {
	default:
		panic("Dust/crypting: bad serverInfo type")

	case *Public:
		inKdfPrefix = kdfS2C
		outKdfPrefix = kdfC2S
		sd.WriteSecret(cs.localPrivate.SharedSecret(sinfo.Key))
		confInput = bytes.Join([][]byte{
			sinfo.Id[:],
			sinfo.Key.Binary(),
			cs.localPrivate.Public.Binary(),
			cs.remotePublic.Binary(),
		}, []byte{})

	case *Private:
		inKdfPrefix = kdfC2S
		outKdfPrefix = kdfS2C
		sd.WriteSecret(sinfo.Key.SharedSecret(cs.remotePublic))
		confInput = bytes.Join([][]byte{
			sinfo.Id[:],
			sinfo.Key.Public.Binary(),
			cs.remotePublic.Binary(),
			cs.localPrivate.Public.Binary(),
		}, []byte{})
	}

	sd.Write(confInput)
	secret := sd.Finish()

	outConfirmation := prim.GenerateMAC(confInput, 0, secret.DeriveAuthKey(outKdfPrefix+kdfMACConfirm))
	outCSlice := outConfirmation[:]
	buf.CopyReassemble(&cs.outCrypted, &outCSlice)
	log.Debug("-> send confirmation starting with %v", outConfirmation[:2])
	cs.outCipher.SetKey(secret.DeriveCipherKey(outKdfPrefix + kdfCipherData))
	cs.outMAC.SetKey(secret.DeriveAuthKey(outKdfPrefix + kdfMACData))
	cs.outMAC.Reset(0)
	cs.inConfirmation = prim.GenerateMAC(confInput, 0, secret.DeriveAuthKey(inKdfPrefix+kdfMACConfirm))
	log.Debug("  <- expect confirmation starting with %v", cs.inConfirmation[:2])
	cs.inCipher.SetKey(secret.DeriveCipherKey(inKdfPrefix + kdfCipherData))
	cs.inMAC.SetKey(secret.DeriveAuthKey(inKdfPrefix + kdfMACData))
	cs.inMAC.Reset(0)

	cs.inHandshake = buf.BeginReassembly(32)
	cs.state = stateHandshakeKey
}

// Change to the Streaming state if we've reassembled a correct confirmation code.  Otherwise, throw away
// the 32-byte chunk and continue.
func (cs *Session) checkConfirmation() {
	if !cs.inConfirmation.Equal(cs.inHandshake.Data()) {
		//log.Debug("<- discarding not-a-confirmation")
		cs.inHandshake.Reset()
		return
	}

	// We already set up all the derived keys when we received the ephemeral key.
	log.Debug("  <- entering streaming state")
	cs.inHandshake = nil
	cs.inStreaming = buf.BeginReassembly(cs.MTU + frameOverhead)
	cs.inPosition = 0
	// Do not reset outPosition here; we may already have been streaming on the output side.
	cs.inFrameStart = 0
	cs.state = stateStreaming
	return
}
