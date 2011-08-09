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

from shared import pump

from dust.extensions.lite.lite_socket2 import lite_socket, makeSession, makeEphemeralSession
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

  coder=yield handshake(coder, client)

  monocle.launch(pump, conn, client, coder.encrypt)
  yield pump(client, conn, coder.decrypt)

@_o
def handshake(coder, client):
  yield client.write(coder.outIv)

  iv=yield client.read(IV_SIZE)
  coder.setIV(iv)

  ekeypair=coder.createEphemeralKeypair()

  yield conn.write(ekeypair.public)
  epub=yield conn.read(KEY_SIZE)

  esession=makeEphemeralSession(ekeypair, epub)

  newCoder=lite_socket(esession)
  newCoder.setIV(ivIn)

  yield Return(newCoder)

add_service(Service(handle_socksDust, port=7050))
eventloop.run()
