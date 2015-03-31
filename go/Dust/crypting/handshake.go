package crypting

import (
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

	var ntor prim.NtorHandshake
	var inKdfPrefix, outKdfPrefix string

	switch sinfo := cs.serverInfo.(type) {
	default:
		panic("Dust/crypting: bad serverInfo type")

	case *Public:
		// We're the client.
		inKdfPrefix, outKdfPrefix = kdfS2C, kdfC2S
		ntor.Init(sinfo.Id[:], &cs.localPrivate.Public, &cs.remotePublic)
		ntor.WriteDHPart(cs.localPrivate.SharedSecret(cs.remotePublic))
		ntor.WriteDHPart(cs.localPrivate.SharedSecret(sinfo.Key))

	case *Private:
		inKdfPrefix, outKdfPrefix = kdfC2S, kdfS2C
		ntor.Init(sinfo.Id[:], &cs.remotePublic, &cs.localPrivate.Public)
		ntor.WriteDHPart(cs.localPrivate.SharedSecret(cs.remotePublic))
		ntor.WriteDHPart(sinfo.Key.SharedSecret(cs.remotePublic))
	}

	var secret prim.Secret
	var outConfirmation prim.AuthValue
	secret, cs.inConfirmation, outConfirmation = ntor.Finish(inKdfPrefix, outKdfPrefix)

	cs.outCrypted.CopyIn(outConfirmation[:])
	log.Debug("-> send confirmation starting with %x", outConfirmation[:2])
	cs.outCipher.SetKey(secret.DeriveCipherKey(outKdfPrefix + kdfCipherData))
	cs.outMAC.SetKey(secret.DeriveAuthKey(outKdfPrefix + kdfMACData))
	cs.outMAC.Reset(0)

	log.Debug("  <- expect confirmation starting with %x", cs.inConfirmation[:2])
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
