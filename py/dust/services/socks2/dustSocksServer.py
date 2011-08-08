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
  myAddr=conn._stack_conn.iostream.socket.getsockname()
  dest=conn._stack_conn.iostream.socket.getpeername()

  sessionKey=makeSession(myAddr, dest)
  coder=lite_socket(sessionKey)

  coder=yield handshake(coder, conn)

  buffer=FakeSocket()

  monocle.launch(pump, conn, buffer, coder.decrypt)
  monocle.launch(handle_socks, buffer.invert())
  yield pump(buffer, conn, coder.encrypt)

@_o
def handshake(coder, conn):
  ivIn=yield conn.read(IV_SIZE)
  coder.setIVIn(ivIn)

  yield conn.write(coder.ivOut)

  ekeypair=coder.createEphemeralKeypair()

  yield conn.write(ekeypair.public.bytes)
  epub=yield conn.read(KEY_SIZE)

  esession=makeEphemeralSession(ekeypair, epub).bytes
  newCoder=lite_socket(esession, ivIn=coder.ivIn, ivOut=coder.ivOut)

#  yield Return(coder)
  yield Return(newCoder)

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
  monocle.launch(pump, conn, client, None)
  yield pump(client, conn, None)

add_service(Service(handle_dust, port=7050))
eventloop.run()
