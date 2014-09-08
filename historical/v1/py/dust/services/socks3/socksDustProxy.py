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

from dust.extensions.lite.lite_socket2 import lite_socket, makeSession, makeEphemeralSession, createEphemeralKeypair
from dust.core.dust_packet import IV_SIZE, KEY_SIZE

@_o
def handle_socksDust(conn):
  print('connection')
  client = Client()
  yield client.connect('blanu.net', 7051)

  coder=yield handshake(client)

  monocle.launch(pump, conn, client, coder.encrypt)
  yield pump(client, conn, coder.decrypt)

@_o
def handshake(conn):
  ekeypair=createEphemeralKeypair()

  yield conn.write(ekeypair.public.bytes)
  epub=yield conn.read(KEY_SIZE)

  esession=makeEphemeralSession(ekeypair, epub)

  coder=lite_socket(esession)

  yield Return(coder)

add_service(Service(handle_socksDust, port=7050))
eventloop.run()
