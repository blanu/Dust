/*
Package Dust implements the main Dust protocol codec for TCP/IP.

To use this library, use LoadCryptoServerIdentityBridgeLine or LoadCryptoServerPrivateFile to acquire the
public or private side of a server identifier, respectively.  For each connection desired, construct a decoder
and encoder for the prearranged shaping model, then use BeginDustClient or BeginDustServer to establish a new
session, which you may use to exchange streams of octets.  Dust sessions will normally continue running in the
background for the duration provided by the model encoder.
*/
package Dust
