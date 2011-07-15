import sys
import time

from struct import unpack
from socket import inet_ntoa

import monocle
from monocle import _o, Return
monocle.init('tornado')

from monocle.stack import eventloop
from monocle.stack.network import add_service, Service, Client, ConnectionLost

@_o
def handle_socks(conn):
  print('connection')
  yield readHandshake(conn)
  yield sendHandshake(conn)
  dest=yield readRequest(conn)
  yield sendResponse(dest, conn)

  addr, port=uncompact(dest)
  print(addr)
  print(port)

  client = Client()
  yield client.connect(addr, port)
  monocle.launch(pump, conn, client)
  yield pump(client, conn)

def uncompact(x):
    ip, port = unpack("!4sH", x)
    return inet_ntoa(ip), port

@_o
def readHandshake(input):
  version=yield input.read(1)
  nauth=yield input.read(1)
  cauth=yield input.read(1)

@_o
def sendHandshake(output):
  yield output.write(b"\x05")
  yield output.write(b"\x00")

@_o
def readRequest(input):
  version=yield input.read(1)
  command=yield input.read(1)
  reserved=yield input.read(1)
  addrtype=yield input.read(1)
  dest=yield input.read(6)

  yield Return(dest)

@_o
def sendResponse(dest, output):
  yield output.write(b"\x05")
  yield output.write(b"\x00")
  yield output.write(b"\x00")
  yield output.write(b"\x01")
  yield output.write(dest)

@_o
def pump(input, output):
  while True:
    try:
      message = yield input.read_some()
    except ConnectionLost:
      output.close()
      break
    except IOError:
      output.close()
      break

    try:
      yield output.write(message)
    except ConnectionLost:
      input.close()
      break
    except IOError:
      input.close()
      break

add_service(Service(handle_socks, port=7050))
eventloop.run()
