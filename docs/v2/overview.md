# Overview of Dust 2.5

## Terminology

[XXX]

## The protocol stack

The Dust 2.5 stack consists of the following context assumptions and resultant layers:

- A client and server (the communicating parties) wish to transceive datagrams.  The client has been introduced
  to the server out of band, but not necessarily the other way around.  The client is already capable of
  establishing “visible stream” connections with the server, where visible streams must conform to certain
  static and statistical properties to avoid being blocked by a censoring adversary.
- The [crypting.md](crypting layer) converts the datagrams into uniform streams to prevent adversaries from
  reading or tampering with them and to eliminate any predictable patterns within the content that adversaries
  might use to detect the presence of a Dust stream.
- The [shaping.md](shaping layer) converts the uniform streams into visible streams that satisfy the desired
  properties so that the censoring adversary, who can see the contents and timing of the visible streams, will
  allow them through.

## The pluggable transport for Tor

The Dust2 pluggable transport for Tor additionally has the following provisions:

- Visible connections are transceived over TCP/IP.
- One Tor bridge connection is transceived per visible connection.  The only transformation made to
  content in each direction is reblocking the byte stream into datagrams of at most the Dust MTU.
