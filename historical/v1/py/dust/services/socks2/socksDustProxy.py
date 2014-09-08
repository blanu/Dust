import sys
import time

from struct import unpack
from socket import inet_ntoa

import monocle
from monocle import _o, Return
monocle.init('tornado')

from monocle.stack import eventloop
from monocle.stack.network import add_service, Service, Client

from dust.core.util import getPublicIP, encode

from shared import pump

from dust.extensions.lite.lite_socket2 import lite_socket, makeSession, makeEphemeralSession, createEphemeralKeypair
from dust.core.dust_packet import IV_SIZE, KEY_SIZE

from socks import *

@_o
def handle_socks(conn):
  yield readHandshake(conn)
  yield sendHandshake(conn)
  dest=yield readRequest(conn)
  yield sendResponse(dest, conn)

  addr, port=uncompact(dest)
  print(addr)
  print(port)

  client = Client()
  yield client.connect(addr, port)

  yield handle_socksDust(conn, client)

@_o
def handle_socksDust(conn, client):
  print('handle socks dust')
  coder=yield handshake(client)

  monocle.launch(pump, conn, client, coder.encrypt)
  yield pump(client, conn, coder.decrypt)

@_o
def handshake(client):
  ekeypair=createEphemeralKeypair()

  yield client.write(ekeypair.public.bytes)
  epub=yield client.read(KEY_SIZE)

  esession=makeEphemeralSession(ekeypair, epub)
  print('esession: '+encode(esession.bytes))
  coder=lite_socket(esession)

  yield Return(coder)

add_service(Service(handle_socks, port=7050))
eventloop.run()
