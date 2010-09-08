import os
import socket
import select
import utp.utp_socket as utp

if os.name == "nt":
  from errno import WSAEMSGSIZE as EMSGSIZE
  from errno import WSAECONNRESET as ECONNRESET
  from errno import WSAEWOULDBLOCK as EWOULDBLOCK
else:
  from errno import EMSGSIZE
  from errno import ECONNRESET
  from errno import EWOULDBLOCK


class FileSendCallbacks(utp.Callbacks):

  def on_read(self, data):
    print("on_read", len(data))

  def on_write(self, count):
    print("on_write", count)
    return self.file.read(count)

  def get_rb_size(self):
    print("get_rb_size")
    return 0

  def on_state(self, state):
    print("on_state", state)
    if state == utp.CONNECT or state == utp.WRITABLE:
      to_write = self.file_size - self.file.tell()
      if self.utp_socket.write(to_write):
        print("upload complete")
        self.utp_socket.close()
        self.file.close()
        self.file = None
    elif state == utp.DESTROYING:
      print("utp_socket is destroyed!")
      self.utp_socket = None

  def on_error(self, errcode):
    print("on_error", errcode)

  def on_overhead(self, send, count, type):
    #print("on_overhead", send, count, type)
    pass


udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
udp_socket.bind(('127.0.0.1', 0))
udp_socket.setblocking(0)
size = 2 * 1024 * 1024
udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, size)
udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_SNDBUF, size)


import sys
sys.path.append(r'C:\Users\Greg\projects\Dust\py')
from extensions.lite.lite_socket import lite_socket
from crypto.keys import KeyManager
dust = lite_socket(KeyManager())
dust.setAddress(udp_socket.getsockname())
print(udp_socket.getsockname())

# dust is the opposite of dirty
def dustPacket(addr, data):
  return dust.encodePacket(addr, data).packet

# dirty is the opposite of dust
def dirtyPacket(addr, data):
  return dust.decodePacket(addr, data).data


def send_to(data, addr):
  data = dustPacket(addr, data)
  try:
      udp_socket.sendto(data, 0, addr)
  except socket.error as se:
      if se.args[0] == EWOULDBLOCK:
          # TODO: UdpOutgoing style buffer
          return

utp_socket = utp.Socket()
utp_socket.init_outgoing(send_to, ("127.0.0.1", 9000))
print("connecting", utp_socket, utp_socket.getpeername())

callbacks = FileSendCallbacks()
utp_socket.set_callbacks(callbacks)

callbacks.utp_socket = utp_socket
callbacks.file = open("foo", "rb")
callbacks.file.seek(0, os.SEEK_END)
callbacks.file_size = callbacks.file.tell()
callbacks.file.seek(0, os.SEEK_SET)

utp_socket.connect()

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
      utp.IsIncomingUTP(None, send_to, data, addr)
  for s in e:
    print("socket error!", s)

while callbacks.utp_socket:
  udp_select(udp_socket, 0.050)
  utp.CheckTimeouts()
