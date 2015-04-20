// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

/*
Package Dust implements the main Dust protocol suite for TCP/IP.

At initialization time, use RegisterModel to associate model names with model constructors.  Attempting to
register the same model name multiple times will panic.

To connect to a Dust server, use LoadServerPublicBridgeLine to load its public server identity.  To act as
a Dust server, use LoadServerPrivateFile to load your private server identity.  Then, use one of the following
modes.  (TODO: Or, you _would_ be able to use one of the following modes, if they all existed.)

Raw mode: as either client or server, you must provide suitable sockets to use directly (currently, TCP
connections).  Use BeginRawClient or BeginRawServer; the Dust code will take over the socket.  You can then
read and write datagrams until the model in use deems that the underlying connection should be closed.  You
probably don't want this mode for most applications.

Message mode: you must provide a way of dialing or accepting suitable sockets.  After BeginMessageClient or
BeginMessageServer, you can read and write datagrams across an entire group of underlying connections.  For
the client, read datagrams can be associated with each other and with previously written datagrams, but you
cannot control the association of datagrams that you write.  For the server, read datagrams can be associated
with each other, but written datagrams must be addressed to a specific client.

Stream mode: you must provide a way of dialing or accepting suitable sockets.  After BeginStreamClient or
BeginStreamServer, you can dial or accept streams across an entire group of underlying connections.  Data
within a stream is associated regardless of the underlying Dust connections it is transceived on.
*/
package Dust

import (
	"github.com/op/go-logging"
)

var log = logging.MustGetLogger("Dust")

var LogModules = []string{
	"Dust",
	"Dust/crypting",
	"Dust/shaping",
}
