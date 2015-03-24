# Overview of Dust for Go

(This is still in an unstable state!  It is not suitable for production deployment.)

## Using Dust

An application that wishes to use Dust will generally link with the main `Dust` package plus one or more model
packages.  By convention:

0. Model codecs are generated as Go code, usually via the programs in `go-modelgen`.  They are placed in a package or packages in the namespace of the client application.
1. A client application imports all of its model packages for side effects.
2. The model packages register their names with a registry in the Dust package at init time.
3. The client application then requests models by short-string names.

Things a client application does _not_ need from this directory:

- `DustModel` contains the canonical locations of various support packages for model implementations.  It does not contain any usable model implementations itself, except for contrived models strictly for testing purposes.
- `cmd` contains invokable support programs for Dust.

## Server identities

This Dust package is loosely designed to use Tor bridge line format for public server identities, and a simple textual format for private identity files.

A bridge line consists of an (uninterpreted) nickname, a TCP/IP endpoint address, and a set of `KEY=VALUE` parameters.  Parameter keys interpreted by Dust include:

- `p=PUBLIC-KEY` (required): the uniform representative of the long-term Curve25519 public key of the server in Base32.
- `m=MODEL-NAME` (required): the model name being requested, out of an application-specific set.
- `m.KEY=VALUE`: possible model-specific parameters.
- `mtu=DECIMAL`: maximum size of datagrams that can be delivered over each Dust connection.  The default is 1500.

Parameter keys that have a question mark as the last character are considered optional hints and may be discarded by versions or configurations of software that do not support them without causing connection failures.

A private identity file consists of the following lines, each terminated by LF or CR LF:

1. Format-identifying magic text: `!!Dust-Server-Private!!`
2. An arbitrary nickname.
3. The TCP/IP endpoint address.
4. The long-term Curve25519 *private* key of the server in Base32.  Currently, there is no provision for encrypting this key, so identity files must be strongly protected at the storage level.
5. One or more lines corresponding to bridge line parameters, excluding the `p` parameter.

Server identities can be manipulated using the DustTool program (in `cmd/DustTool`).
