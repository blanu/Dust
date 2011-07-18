import sys
import time

from struct import unpack
from socket import inet_ntoa

import monocle
from monocle import _o, Return
monocle.init('tornado')

from monocle.stack import eventloop
from monocle.stack.network import add_service, Service, Client

from shared import pump, DustCoder

@_o
def handle_socksDust(conn):
  print('connection')
  client = Client()
  yield client.connect('blanu.net', 7051)

  myAddr=client._stack_conn.iostream.socket.getsockname()
  dest=client._stack_conn.iostream.socket.getpeername()
  coder=DustCoder(myAddr, dest)

  monocle.launch(pump, conn, client, coder.dustPacket)
  yield pump(client, conn, coder.dirtyPacket)

add_service(Service(handle_socksDust, port=7050))
eventloop.run()
