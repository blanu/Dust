# Ensemble layer

(This layer isn't implemented!  This is a protocol sketch.)

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

