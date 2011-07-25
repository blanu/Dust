import sys
import time

from struct import unpack
from socket import inet_ntoa

import monocle
from monocle import _o, Return
monocle.init('tornado')

from monocle.stack import eventloop
from monocle.stack.network import add_service, Service, Client

from dust.core.util import getPublicIP

from shared import pump, DustCoder

from dust.extensions.lite_socket2 import lite_socket, makeSession
from dust.core.dust_packet import IV_SIZE

@_o
def handle_socksDust(conn):
  print('connection')
  client = Client()
  yield client.connect('blanu.net', 7051)

  myAddr=client._stack_conn.iostream.socket.getsockname()
  myAddr=(getPublicIP(v6=False), myAddr[1])
  dest=client._stack_conn.iostream.socket.getpeername()
  print('dest: '+str(dest))

  sessionKey=makeSession(myAddr, dest)
  coder=lite_socket(sessionKey)

  handshake(coder, client)

  monocle.launch(pump, conn, client, coder.encrypt)
  yield pump(client, conn, coder.decrypt)

def handshake(coder, client):
  yield client.write(coder.outIv)

  iv=yield client.read(IV_SIZE)
  coder.setIV(iv)

add_service(Service(handle_socksDust, port=7050))
eventloop.run()
