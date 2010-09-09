import os
import time
import socket
import select
import utp.utp_socket as utp

if os.name == "nt":
  from errno import WSAEMSGSIZE as EMSGSIZE
  from errno import WSAECONNRESET as ECONNRESET
else:
  from errno import EMSGSIZE
  from errno import ECONNRESET
  from errno import EAGAIN
  from errno import ETIMEDOUT, EINTR, EWOULDBLOCK

class FileRecvCallbacks(utp.Callbacks):
  def on_read(self, data):
    print("on_read", len(data))
    self.file.write(data)
    self.file.flush()

  def on_write(self, count):
    print("on_write", count)
    assert False

  def get_rb_size(self):
    print("get_rb_size")
    return 0

  def on_state(self, state):
    print("on_state", state)
    if state == utp.EOF:
      print("EOF")
      self.utp_socket.close()
    elif state == utp.DESTROYING:
      print("utp_socket is destroyed!")
      self.utp_socket = None

  def on_error(self, errcode):
    print("on_error", errcode)
    self.utp_socket.close()

  def on_overhead(self, send, count, type):
    #print "on_overhead", send, count, type
    pass

callbacks = False

def got_incoming_connection(utp_socket):
  global callbacks
  print("got_incoming_connection!", utp_socket, utp_socket.getpeername())

  callbacks = FileRecvCallbacks()
  utp_socket.set_callbacks(callbacks)

  callbacks.utp_socket = utp_socket
  callbacks.file = open("foo.recv", "wb")

udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
udp_socket.bind(('127.0.0.1', 9000))
udp_socket.setblocking(0)
size = 2 * 1024 * 1024
udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, size)
udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_SNDBUF, size)

import sys
sys.path.append(r'C:\Users\Greg\projects\Dust\py')
from dust.extensions.lite.lite_socket import lite_socket
from dust.crypto.keys import KeyManager
dust = lite_socket(KeyManager())
dust.setAddress(udp_socket.getsockname())
print(udp_socket.getsockname())

# dust is the opposite of dirty
def dustPacket(addr, data):
  print('dust')
#  return dust.encodePacket(addr, data).packet
  print(str(data))
  return data

# dirty is the opposite of dust
def dirtyPacket(addr, data):
#  return dust.decodePacket(addr, data).data
  print('dirty')
  print(str(data))
  return data

def send_to(data, addr):
  # TODO: UdpOutgoing style buffer
  data = dustPacket(addr, data)
  udp_socket.sendto(data, 0, addr)

def udp_select(udp_socket, timeout):
  r, w, e = select.select([udp_socket], [], [udp_socket], timeout)
  if not r and not e:
    return
  # TODO: UdpOutgoing style buffer
  #udp_flush(udp_socket)
  for s in r:
    while True:
      try:
        data, addr = s.recvfrom(8192)
      except socket.error as se:
        if se.args[0] in (ECONNRESET, EMSGSIZE):
          continue
        break
      data = dirtyPacket(addr, data)
      utp.IsIncomingUTP(got_incoming_connection, send_to, data, addr)
  for s in e:
    print("socket error!"+str(s))

while callbacks is False or callbacks.utp_socket:
  udp_select(udp_socket, 0.050)
  utp.CheckTimeouts()
