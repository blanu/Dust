from struct import unpack
from socket import inet_ntoa

import monocle
from monocle import _o, Return

from dust.core.util import encode

def uncompact(x):
    ip, port = unpack("!4sH", x)
    return inet_ntoa(ip), port

@_o
def readHandshake(input):
  print('readHandshake')
  version=yield input.read(1)
  print('version: '+encode(str(version)))
  nauth=yield input.read(1)
  nauth=unpack('B', nauth)[0]
  print('nauth: '+encode(str(nauth)))
  auths=[]
  for x in range(nauth):
    auth=yield input.read(1)
    auth=unpack('B', auth)[0]
    auths.append(auth)
  print('auths: '+encode(str(auths)))
  print('1!')

@_o
def sendHandshake(output):
  print('sendHandshake')
  yield output.write(b"\x05")
  yield output.write(b"\x00")
  print('2@')

@_o
def readRequest(input):
  version=yield input.read(1)
  command=yield input.read(1)
  reserved=yield input.read(1)
  addrtype=yield input.read(1)
  dest=yield input.read(6)
  print('3#')

  yield Return(dest)

@_o
def sendResponse(dest, output):
  yield output.write(b"\x05")
  yield output.write(b"\x00")
  yield output.write(b"\x00")
  yield output.write(b"\x01")
  yield output.write(dest)
  print('4$')
