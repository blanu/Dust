import sys
import time

from struct import unpack
from socket import inet_ntoa

import monocle
from monocle import _o, Return
monocle.init('tornado')

from monocle.stack import eventloop
from monocle.stack.network import add_service, Service, Client

from dust.extensions.lite.lite_socket import lite_socket
from dust.crypto.keys import KeyManager

from shared import pump, DustCoder

myAddr=('localhost', 7050)
addr='localhost'
port=7051
dest=(addr, port)

duster=lite_socket(KeyManager())
duster.setAddress(myAddr)

coder=DustCoder(duster, dest)

@_o
def handle_socksDust(conn):
  print('connection')
  print(conn)
  client = Client()
  yield client.connect(addr, port)
  monocle.launch(pump, conn, client, coder.dustPacket)
  yield pump(client, conn, coder.dirtyPacket)

add_service(Service(handle_socksDust, port=7050))
eventloop.run()
