import os
import sys
import time
import socket
import select
from queue import Queue, Empty

import utp.utp_socket as utp

if os.name == "nt":
  from errno import WSAEMSGSIZE as EMSGSIZE
  from errno import WSAECONNRESET as ECONNRESET
  from errno import WSAEWOULDBLOCK as EWOULDBLOCK
else:
  from errno import EMSGSIZE
  from errno import ECONNRESET
  from errno import EWOULDBLOCK

class UtpSocket(utp.Callbacks):
  def __init__(self, server, inq, outq):
    utp.Callbacks.__init__(self)
    self.server=server
    self.inq=inq
    self.outq=outq
    self.done=False
    self.bufferQueue=Queue()
    self.buffer=b''
    self.connected=False

    self.udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    if self.server:
      self.udp_socket.bind(('127.0.0.1', 9000))
    else:
      self.udp_socket.bind(('127.0.0.1', 0))
    self.udp_socket.setblocking(0)

    size = 2 * 1024 * 1024
    self.udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, size)
    self.udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_SNDBUF, size)

    self.utp_socket = utp.Socket()
    self.utp_socket.init_outgoing(self.send_to, ("127.0.0.1", 9000))
    print("connecting", self.utp_socket, self.utp_socket.getpeername())

    if not server:
      self.utp_socket.set_callbacks(self)
      self.utp_socket.connect()

  def start(self):
    while self.utp_socket:
      if self.connected:
        self.processInQueue()
      self.udp_select(0.050)
      utp.CheckTimeouts()

  def processInQueue(self):
    try:
      data=self.inq.get_nowait()
    except Empty:
      return

    if data:
      self.bufferQueue.put(data)
      self.utp_socket.write(len(data))
    else:
      self.done=True
      self.utp_socket.close()
      return

  def send_to(self, data, addr):
    print('send_to: '+str(len(data)))
    try:
      self.udp_socket.sendto(data, 0, addr)
    except socket.error as se:
      print('socket error: '+str(se))

  def got_incoming_connection(self, utp_socket):
    print("got_incoming_connection!", utp_socket, utp_socket.getpeername())
    self.utp_socket.set_callbacks(self)

  def udp_select(self, timeout):
    r, w, e = select.select([self.udp_socket], [], [self.udp_socket], timeout)
    if not r and not e:
      return
    for s in r:
      while True:
        try:
          data, addr = s.recvfrom(8192)
        except socket.error as se:
          if se.args[0] in (ECONNRESET, EMSGSIZE):
            continue
          break
        if self.server:
          utp.IsIncomingUTP(self.got_incoming_connection, self.send_to, data, addr)
        else:
          utp.IsIncomingUTP(None, self.send_to, data, addr)
    for s in e:
      print("socket error!", s)

  # uTP Callbacks
  def on_read(self, data):
    print("on_read", len(data))
    self.outq.put(data)

  def on_write(self, count):
    print("on_write", count)
    data=b''
    while len(data)<count: # Fill data up to count
      if len(self.buffer)>0: # Is there data in the buffer?
        if count>=len(self.buffer): # We can use all the buffer
          data=data+self.buffer
          self.buffer=''
        else: # We only need part of the buffer
          data=data+self.buffer[:count]
          self.buffer=self.buffer[count:]
      else: # No data in buffer, check queue
        try:
          buff=self.bufferQueue.get_nowait()
        except: # No data in queue
          print('No more data buffered')
          break
        self.buffer=self.buffer+buff # Move from queue to buffer, repeat
    return data

  def get_rb_size(self):
    print("get_rb_size")
    return 0

  def on_state(self, state):
    print("on_state", state)
    if state == utp.CONNECT or state == utp.WRITABLE:
      print('CONNECT or WRITABLE')
      self.connected=True
    elif state == utp.DESTROYING:
      print('DESTROYING')
      print("utp_socket is destroyed!")
      self.utp_socket = None
      self.connected=False

  def on_error(self, errcode):
    print("on_error", errcode)
    self.utp_socket.close()

  def on_overhead(self, send, count, type):
    #print("on_overhead", send, count, type)
    pass

if __name__=='__main__':
  from queue import Queue

  server=sys.argv[1]=='-s'

  inq=Queue()
  outq=Queue()

  inq.put(b'test')

  us=UtpSocket(server, inq, outq)
  us.start()
