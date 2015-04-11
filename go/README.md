# Overview of Dust for Go

(This is still in an unstable state!  It is not suitable for production deployment.)

## Using Dust

An application that wishes to use Dust will generally link with the main Dust engine package plus one or more model packages.  By convention:

0. Model codecs are generated as Go code, usually via the programs in ../modelgen.  They are placed in a package or packages in the namespace of the client application.
1. A client application imports all of its model packages for side effects.
2. The model packages call into the Dust package to register their names at init time.
3. The client application can then request models by short-string names for establishing connections.

Currently, the only available Dust engine package is in ./v2/engine; its full import path is therefore github.com/blanu/Dust/go/v2/engine.

## Server identities in Dust 2

The Dust engine uses ASCII key-value parameters for representing public and private server identity and endpoint configuration information.  Keys that have a question mark as the last character are considered optional hints; these may be discarded by software versions or configurations that do not support them, without causing connection failures.  Neither a key nor a value may contain newlines.  A key may not contain horizontal whitespace or the # character.  A value may not begin with horizontal whitespace.

Endpoint configuration parameters used by either side include:

- `m=MODEL-NAME` (required): the model name being requested, out of an application-specific set.
- `m.KEY=VALUE`: possible model-specific parameters.
- `mtu=DECIMAL`: maximum size of datagrams that can be delivered over each Dust connection.  The default is 1500.

Parameters given to a client to identify a server also include:

- `p=PUBLIC-KEY` (required): the uniform representative of the long-term Curve25519 public key of the server in Base32.
- `n=OPAQUE` (required): the 32-octet opaque name of the server in Base32.

Parameters given to a server to identify itself instead include:

- `px!=PRIVATE-KEY` (required): the long-term Curve25519 private key of the server in Base32.
- `n=OPAQUE` (required): the 32-octet opaque name of the server in Base32.
