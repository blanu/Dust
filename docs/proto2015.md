[WORKING COPY]

# Syntax of this document

[XXX]

# The Dust stack

The “short” Dust stack consists of the following layers:

- A client and server (the communicating parties) wish to transceive datagrams.  The client has been introduced
  to the server out of band, but not necessarily the other way around.  The client is already capable of
  establishing “visible stream” connections with the server, where visible streams must conform to certain
  static and statistical properties to avoid being blocked by a censoring adversary.
- The crypting layer converts the datagrams into uniform streams to prevent adversaries from reading or
  tampering with them and to eliminate any predictable patterns within the content that adversaries might
  use to detect the presence of a Dust stream.
- The shaping layer converts the uniform streams into visible streams that satisfy the desired properties
  so that the censoring adversary, who can see the contents and timing of the visible streams, will allow
  them through.

The “tall” Dust stack adds an additional layer:

- Instead of wishing to transceive datagrams directly, the client and server wish to transceive one or more
  tunneled byte stream connections, which may be longer-lived and higher-capacity than any individual visible
  connections.
- The ensemble layer provides segmentation, flow control and retransmission, and session resumption for the
  tunneled streams, allowing them to be multiplexed 1:N or M:N over the datagram connections provided by the
  crypting layer.

The tall Dust stack is used for the Tor pluggable transport application of Dust.

# Ensemble layer

## Overview

The ensemble layer mediates between a group of weakly-ordered datagram connections and a group of potentially
longer-lived stream connections.  In the tall Dust stack, it mediates between media datagrams and tunneled
streams.

Tunneled connections require control signaling at this layer because:

- Visible connections must have their statistical properties controlled by the shaping model.  In particular,
  allowing tunneled connection backpressure to propagate to backpressure at the visible connection layer may
  cause the transceivers for that layer to act outside the model.
- Tunneled connections may require higher capacities or durations than each visible connection can permit.
  This means that their contents must be able to be split over multiple visible connections.  (It may or may
  not be desirable to let a single visible connection handle multiple tunneled connections.)

## Attached connections and partials

[XXX]

## Framing and commands

Each datagram within a partial consists of zero or more concatenated commands.  Commands are processed in
order; if an invalid command is encountered, [XXX: etc.].  The set of byte strings representing syntactically
valid commands is not prefix-free, but if S represents a syntactically valid command, and there exists any S′
such that S is a strict prefix of S′ and S′ also represents a syntactically valid command, then _all_ byte
strings beginning with S represent syntactically valid commands; this is used to let commands extend “to the
end of the datagram” without requiring additional framing.

Commands include:

- 80: (Nop) No effect.
- 93 [32]ID: (Bind) The attached-connection ID for this partial is set to the ID parameter for all future
  commands.  The send position of this partial is unset.  Implies a request to set the same ID for the
  counterpartial.
- 06 [4]Pos [2]Win: (Ack) All bytes before Pos have been received for the receive half-connection, and the
  remote may send up to Win more bytes starting at Pos.  If this is sent on a c2s partial, and the attached
  connection is inactive, this implies a request to establish the attached connection.
- 02 [4]Pos: (Seek) The send position for this partial is now Pos.
- 10 [*]Text: (Data) The data for the send half-connection starting at the send position starts with Text.
  The parameter may be of any length and is terminated at the end of the datagram.
- 03: (Fin) No further data will ever be sent on the send half-connection at or after the send position.
- 15: (Nak) No further data will ever be accepted on the receive half-connection at or after the last position
  acknowledged.
- 1A: (Resync) Some datagrams were dropped from the counterpartial; all state for the counterpartial should
  be retransmitted.

Over a datagram channel which can transport datagrams at least 47 bytes long, given an established attached
connection, it is always possible to make at least one byte of progress per datagram regardless of
synchronization state, by concatenating Bind, Seek, Ack, and Data commands (replacing Data with Fin as
needed).

# Crypting layer

## Overview

The crypting layer mediates between an insecure client-server stream connection and a secure weakly-ordered
datagram connection.  In the Dust stack, it mediates between uniform streams and media datagrams.  The
crypting layer has the following design properties:

* Datagrams are authenticated and encrypted in both directions: a datagram traveling in either direction
  is not forgeable or readable by a party which is not either the client or server.
* The server is preauthenticated to the client.
* A newly-connecting client is not authenticated to the server, but within the same stream connection, all
  datagrams are known to the server to be associated with the same client.
* The probability distribution of all bytes in both directions of the stream connection from the perspective
  of any outside observer is uniform, independent within a connection, and independent across connections.
  [XXX: Is that written too strongly?]
* The chatterbox constraint: each sending side in the stream connection never “blocks”.  That is: for any
  observed-tuple (R, S), R being the received and S being the sent byte strings so far from the perspective
  of one endpoint of the stream connection, the endpoint can construct a nonempty string P such that
  (R, S‖P) is semantically equivalent to (R, S).  (Note that this implies that for any extension length N,
  a suitable P at least N long can be constructed by repeating this process.)
* If D_0, D_1, …, D_n are the datagrams sent in order in one half-connection, the receiving endpoint will
  receive E_0, E_1, …, E_k where k ≤ n and each E_j is either D_j or a distinguishable symbol indicating that
  the datagram numbered j was lost.

## Preparation

A server identity consists of a long-term keypair and an identity string.  Server parameters consist of an MTU
in bytes, 1280 ≤ MTU ≤ 32000.  The server identity and parameters must be known to the client out of band
before any connections can be established.

An identity string is normally constructed from the server's underlying endpoint address by tag-length-value
concatenation of structures describing parts of the endpoint address from outermost to innermost protocol
layer.  Each such structure has the format:

- [1] Tag: usually, the IANA protocol number of the layer being identified.  In particular, this is true for
  IPv4 (4), IPv6 (41), and TCP (6).  The tag 0 means “end of identity”.
- [1] Len: the length of the address part for this layer.
- [Len] Val: the binary-formatted address part for this layer.  For IPv4 or IPv6, this is the 4- or 16-byte
  IP address.  For TCP, this is the 2-byte port number in big-endian order.

The exact format of identity strings for non-TCP/IP addresses is not defined in this document.

Example: the identity string for a server listening on TCP port 16667 at 2001:db8:7ea::ea21:92e9:807 is:

```
  29 10                   # proto 41 (IPv6), 16 bytes of address
  20 01 0D B8 07 EA 00 00 # raw IP address
  00 00 EA 21 92 E9 08 07
  06 02                   # proto 6 (TCP), 2 bytes of address
  41 1B                   # raw port
  00 00                   # no more parts
```

## Handshake

A half-connection starts with a handshake.  The sending endpoint generates an ephemeral keypair, and
transmits:

- [PK_LEN] Ephemeral public key repr
- [MAC_LEN×N] Random bytes, for integer N ≥ 0

The amount of random bytes transmitted is arbitrary at this layer, but an “expandable” component is necessary
here due to the chatterbox constraint.  After the endpoint receives the ephemeral public key from the other
side, it can generate the shared cryptographic values needed:

1. Secret = Digest(input) where input is the concatenation of:
  1. [DH_LEN] DH(server ephemeral, client ephemeral)
  2. [DH_LEN] DH(server long-term, client ephemeral)
  3. [*] Server identity string
  4. [PK_LEN] Server long-term public key repr
  5. [PK_LEN] Client ephemeral public key repr
  6. [PK_LEN] Server ephemeral public key repr
2. FromLocal = `"cli."` for the client, `"srv."` for the server
3. Derived keys:
  - ConfKey = KDF(Secret, `"mac."`‖FromLocal‖`"bgn."`)
  - StreamKey = KDF(Secret, `"clk."`‖FromLocal‖`"dta."`)
  - AuthKey = KDF(Secret, `"mac."`‖FromLocal‖`"grm."`)
4. Confirmation = MAC(input, 0, ConfKey) where input is the concatenation of:
  1. [*] Server identity string
  2. [PK_LEN] Server long-term public key repr
  3. [PK_LEN] Client ephemeral public key repr
  4. [PK_LEN] Server ephemeral public key repr

All steps after the shared secret generation are also computed from the perspective of the other endpoint, for
determining when the confirmation has been received and keying the receiving half-connection.  Once this is
done, endpoint can transmit:

- [MAC_LEN] Confirmation

After these bytes, the sending half-connection enters the streaming state.  The receiving side should
therefore check each chunk of MAC_LEN bytes after the initial public key transmission, and have the receiving
half-connection enter the streaming state when one such chunk matches the confirmation code expected to be
sent from the other side.

## Streaming

A half-connection in the streaming state transmits authenticated data frames arbitrarily interleaved with
padding.  All bytes are encrypted with a stream cipher, keyed with StreamKey as described in the “Handshake”
section; the stream cipher is never rekeyed.  Byte position zero in the half-connection starts immediately
after the handshake.

An authenticated data frame has the structure:

- [2] TLen: Len + 256, where Len is the length of the datagram being transmitted, 0 ≤ Len ≤ MTU ≤ 32000.
- [Len] Data: the raw datagram being transmitted.
- [MAC_LEN] MAC(cpart, start, AuthKey), where:
  - start is the starting byte position in the half-connection for the authenticated region: zero for the
    earliest authenticator transmitted, and immediately after the most recent previous authenticator
    otherwise.
  - cpart is all _encrypted_ bytes from the start position to before this authenticator.
  - AuthKey is as described in the “Handshake” section.

Any number of zero-valued bytes may be inserted as padding between data frames.  The first byte of such a
frame is never 0, because 256 ≤ TLen, so the start position of any subsequent data frame is unambiguous.

## Termination

[XXX etc. etc. high-bit octet at start of frame]

# Shaping layer

## Overview

The shaping layer mediates between a raw stream connection which is expected to transmit statistically uniform
bytes in both directions and a shaped stream connection conforming to the properties of a model.  In the Dust
stack, it mediates between uniform streams and visible streams.
