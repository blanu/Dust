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
  myAddr=conn._stack_conn.iostream.socket.getsockname()
  dest=('blanu.net', 7051)
  coder=DustCoder(myAddr, dest)
  client = Client()
  yield client.connect(dest[0], dest[1])
  monocle.launch(pump, conn, client, coder.dustPacket)
  yield pump(client, conn, coder.dirtyPacket)

add_service(Service(handle_socksDust, port=7050))
eventloop.run()
