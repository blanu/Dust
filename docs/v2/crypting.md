# Crypting (secure channel) layer for Dust 2.5

## Overview

The crypting layer mediates between an insecure client-server stream connection and a secure weakly-ordered
datagram connection.  In the Dust stack, it mediates between uniform stream connections and media datagrams.
The crypting layer has the following design properties:

* Datagrams are authenticated and encrypted in both directions: a datagram traveling in either direction
  is not forgeable or readable by a party which is not either the client or server.
* The server is preauthenticated to the client.
* A newly-connecting client is not authenticated to the server, but within the same stream connection, all
  datagrams are known to the server to be associated with the same client.
* The probability distribution of all bytes in both directions of the stream connection from the perspective
  of any outside observer is uniform, independent within a connection, and independent across connections.
  [XXX: Is that written too strongly?]
* The chatterbox constraint: no stream “blocks”.  That is: for any observed-tuple (R, S), R being the
  received and S being the sent byte strings so far from the perspective of one endpoint of a stream
  connection, the endpoint can construct a nonempty string P such that (R, S‖P) is semantically equivalent
  to (R, S).  (Note that this implies that for any extension length N, a suitable P at least N long can be
  constructed by repeating this process.)
* The erasure channel constraint: if D_0, D_1, …, D_n are the datagrams sent in order in one stream,
  the receiving endpoint will receive E_0, E_1, …, E_k where k ≤ n and each E_j is either D_j or a
  distinguishable symbol indicating that the datagram numbered j was lost.

## Cryptographic primitives

The following functions and constants are assumed:

- NTOR_SEED_LEN, NTOR_PROTO_ID_LEN, SECRET_LEN, MAC_LEN, KEY_LEN, PK_LEN, DH_LEN: positive integers.
- NTOR_PROTO_ID[NTOR_PROTO_ID_LEN]: a constant string.
- NtorDigest2(input[*]) → (seed[NTOR_SEED_LEN] × secret[SECRET_LEN]): a digest function.
- NtorDigestM(input[*]) → confirmation[MAC_LEN]: a digest function.
- KDF(secret[SECRET_LEN], id[*]) → key[KEY_LEN]: a key derivation function.
- MAC(input[*], nonce, key[KEY_LEN]) → mac[MAC_LEN]: a message authentication function.  The nonce is an integer between 0 and 2^64 − 1.
- Stream(key[KEY_LEN]) → stream: a stream cipher.  It should be possible to compute any prefix of the output stream of less than [XXX] bytes.
- NewKey(entropy) → private, public[PK_LEN]: a fast asymmetrical keypair generator suitable for the DH operation.
- DH(private, public[PK_LEN]) → result[DH_LEN]: a Diffie-Hellman-style key agreement primitive.  For all entropy0, entropy1, given (private0, public0) = NewKey(entropy0) and (private1, public1) = NewKey(entropy1), DH(private0, public1) = DH(private1, public0).

[XXX: maximum stream lengths?]

The main ciphersuite for Dust 2.5 uses:

- NTOR_SEED_LEN = SECRET_LEN = MAC_LEN = KEY_LEN = 32.
- NTOR_PROTO_ID = "ntor(Dust2015)", NTOR_PROTO_ID_LEN = 14.
- pprefix = "tag:blanu.net,2015:Dust2015/".  P(suffix) = pprefix‖suffix.
- NtorDigest2(input) = Skein-512-512(input, T_prs: P("ntor-H.skein")).  The seed is the first half of the result; the secret is the second half.
- NtorDigestM(input) = Skein-512-256(input, T_prs: P("ntor-Hmac.skein")).
- KDF(secret, id) = Skein-512-256(ε, T_key: secret, T_kdf: id, T_prs: P("kdf.skein")).  ε is the empty string.
- MAC(input, nonce, key) = Skein-512-256(input, T_key: key, T_non: nonce64, T_prs: P("mac.skein")).  nonce64 is nonce represented in big-endian 64-bit binary form.
- Stream(key) = Skein-512-Cipher(ε, T_key: key, T_prs: P("stream.skein")).  The nominal output length is set to 2^64 − 1 bits, and continuously clocked output from Skein becomes the key stream.  ε is the empty string.
- PK_LEN = 32.  DH_LEN = 32.
- NewKey(entropy) = Elligator-Curve25519.  The public key representation is the Elligator uniform representative.  Multiple tries may be needed to generate a valid Elligator key; the current implementation consumes up to 256*PK_LEN bits from the entropy source.
- DH(private, public) = Elligator-Curve25519-ECDH.

The Skein in use is Skein 1.3 of 2010-10-01.

## Preparation

A server identity consists of a long-term keypair and an identity string.  Server parameters consist of an MTU
in bytes, 1280 ≤ MTU ≤ 32000.  The server identity and parameters must be known to the client out of band
before any connections can be established.  An identity string is normally a 32-byte string randomly chosen
at server initialization time.

## Handshake

A stream starts with a handshake.  The sending endpoint generates an ephemeral keypair, and transmits:

- [PK_LEN] Ephemeral public key repr
- [MAC_LEN×N] Random bytes, for integer N ≥ 0

The amount of random bytes transmitted is arbitrary at this layer, but an “expandable” component is necessary
here due to the chatterbox constraint.  After the endpoint receives the ephemeral public key from the other
side, it can generate the shared cryptographic values needed:

1. (ConfSeed, Secret) = NtorDigest2(input) where input is the concatenation of:
  1. [DH_LEN] DH(server ephemeral, client ephemeral)
  2. [DH_LEN] DH(server long-term, client ephemeral)
  3. [*] Server identity string
  4. [PK_LEN] Client ephemeral public key repr
  5. [PK_LEN] Server ephemeral public key repr
  6. [NTOR_PROTO_ID_LEN] The literal string NTOR_PROTO_ID
2. FromLocal = "cli." for the client or "srv." for the server
3. Confirmation = NtorDigestM(input) where input is the concatenation of:
  1. [NTOR_SEED_LEN] ConfSeed
  2. [*] Server identity string
  3. [PK_LEN] Server ephemeral public key repr
  4. [PK_LEN] Client ephemeral public key repr
  5. [NTOR_PROTO_ID_LEN] The literal string NTOR_PROTO_ID
  6. [4] FromLocal
4. Derived keys:
  - StreamKey = KDF(Secret, "clk."‖FromLocal‖"dta.")
  - AuthKey = KDF(Secret, "mac."‖FromLocal‖"grm.")

All steps after step 1 are also computed from the perspective of the other endpoint, for determining when the
confirmation has been received and keying the receiving stream.  Once this is done, endpoint can transmit:

- [MAC_LEN] Confirmation

After these bytes, the sending stream enters the streaming state.  The receiving side should therefore check
each chunk of MAC_LEN bytes after the initial public key transmission, and have the receiving stream enter the
streaming state when one such chunk matches the confirmation code expected to be sent from the other side.

## Streaming

A stream in the streaming state transmits authenticated data frames arbitrarily interleaved with padding.  All
bytes are encrypted with a stream cipher, keyed with StreamKey as described in the “Handshake” section; the
stream cipher is never rekeyed.  Byte position zero in a stream corresponds to the earliest byte transmitted
after the handshake.

An authenticated data frame has the structure:

- [2] TLen: Len + 256, where Len is the length of the datagram being transmitted, 0 ≤ Len ≤ MTU ≤ 32000.
- [Len] Data: the raw datagram being transmitted.
- [MAC_LEN] MAC(cpart, start, AuthKey), where:
  - start is the starting byte position in the stream for the authenticated region: zero for the
    earliest authenticator transmitted, and immediately after the most recent previous authenticator
    otherwise.
  - cpart is all _encrypted_ bytes from the start position to before this authenticator.
  - AuthKey is as described in the “Handshake” section.

Any number of zero-valued bytes may be inserted as padding between data frames.  The first byte of such a
frame is never 0, because 256 ≤ TLen, so the start position of any subsequent data frame is unambiguous.

## Termination

There is no prescribed way to signal the clean shutdown of data transmission in-band with regard to a uniform
stream; this is left to the next-layer protocol.  Shutting down half of a uniform stream connection out of
band shuts down that stream; it is unspecified whether this will implicitly cause the other side to be shut
down as well.
