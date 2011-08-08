import sys
import time

from struct import unpack
from socket import inet_ntoa

import monocle
from monocle import _o, Return
monocle.init('tornado')

from monocle.stack import eventloop
from monocle.stack.network import add_service, Service, Client, ConnectionLost
from loopback import FakeSocket

from dust.core.dust_packet import IV_SIZE, KEY_SIZE

from dust.extensions.lite.lite_socket2 import lite_socket, makeSession, makeEphemeralSession
from dust.core.util import encode

from shared import *
from socks import *

@_o
def handle_dust(conn):
  print('handle_dust')
  coder=yield handshake(coder, conn)

  client = Client()
#  yield client.connect('localhost', 9050)
  yield client.connect('blanu.net', 80)

  monocle.launch(pump, conn, client, coder.decrypt)
  yield pump(client, conn, coder.encrypt)

@_o
def handshake(coder, conn):
  ivIn=yield conn.read(IV_SIZE)

  yield conn.write(coder.ivOut)

  ekeypair=coder.createEphemeralKeypair()

  yield conn.write(ekeypair.public.bytes)
  epub=yield conn.read(KEY_SIZE)

  esession=makeEphemeralSession(ekeypair, epub).bytes
  coder=lite_socket(esession, ivIn=coder.ivIn, ivOut=coder.ivOut)

#  yield Return(coder)
  yield Return(coder)

add_service(Service(handle_dust, port=7051))
eventloop.run()
