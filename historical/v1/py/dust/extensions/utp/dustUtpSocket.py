import os
import sys
import time
import socket
import select
from queue import Queue, Empty
from threading import Thread

import utp.utp_socket as utp
from dust.crypto.keys import KeyManager
from dust.extensions.lite.lite_socket import lite_socket

if os.name == "nt":
  from errno import WSAEMSGSIZE as EMSGSIZE
  from errno import WSAECONNRESET as ECONNRESET
  from errno import WSAEWOULDBLOCK as EWOULDBLOCK
else:
  from errno import EMSGSIZE
  from errno import ECONNRESET
  from errno import EWOULDBLOCK

class DustUtpSocket(utp.Callbacks):
  def __init__(self, inq, outq):
    utp.Callbacks.__init__(self)
    self.inq=inq
    self.outq=outq

    self.doneIn=False
    self.doneWrite=False
    self.closed=False
    self.bufferQueue=Queue()
    self.buffer=b''
    self.connected=False
    self.connq=Queue()

    self.udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    self.udp_socket.bind(('127.0.0.1', 0))
    self.udp_socket.setblocking(0)

    size = 2 * 1024 * 1024
    self.udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, size)
    self.udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_SNDBUF, size)

    self.utp_socket = utp.Socket()
    print("connecting", self.utp_socket)
    self.dust = lite_socket(KeyManager())
    self.dust.setAddress(self.udp_socket.getsockname())
    print(self.udp_socket.getsockname())

    self.utp_socket.init_outgoing(self.send_to, ("127.0.0.1", 9000))

    self.utp_socket.set_callbacks(self)
    self.utp_socket.connect()

  def start(self):
    t=Thread(target=self.run)
    t.setDaemon(True)
    t.start()

  def run(self):
    while self.utp_socket:
      if self.connected and not self.doneIn:
        self.processInQueue()
      if self.doneWrite and not self.closed:
        print('closing...')
        self.closed=True
        self.utp_socket.close()
      self.udp_select(0.050)
      utp.CheckTimeouts()

  def processInQueue(self):
#    print('processInQueue: '+str(self.inq.qsize()))
    try:
      data=self.inq.get_nowait()
    except Empty:
      return

    if data:
      print('got from queue: '+str(data)+' '+str(len(data)))
      self.bufferQueue.put(data)
      self.connected=self.utp_socket.write(len(data))
    else:
      print('Got None from queue, done writing')
      self.doneIn=True
      self.checkClose()
      return

  def checkClose(self):
    if self.doneIn and len(self.buffer)==0 and self.bufferQueue.qsize()==0:
      print('doneWrite')
      self.doneWrite=True

  # dust is the opposite of dirty
  def dustPacket(self, addr, data):
    print('dust')
#    return self.dust.encodePacket(addr, data).packet
    return data

  # dirty is the opposite of dust
  def dirtyPacket(self, addr, data):
    print('dirty')
#    return self.dust.decodePacket(addr, data).data
    return data

  def send_to(self, data, addr):
    print('send_to: '+str(len(data)))
    data = self.dustPacket(addr, data)
    try:
      self.udp_socket.sendto(data, 0, addr)
    except socket.error as se:
      print('socket error: '+str(se))

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
        data = self.dirtyPacket(addr, data)
        utp.IsIncomingUTP(None, self.send_to, data, addr)
    for s in e:
      print("socket error!", s)

  # uTP Callbacks
  def on_read(self, data):
    print("on_read", len(data))
    self.outq.put(data)
    print('outq: '+str(self.outq.qsize()))
    print(str(data))

  def on_write(self, count):
    print("on_write", count)
    data=b''
    while len(data)<count: # Fill data up to count
      if len(self.buffer)>0: # Is there data in the buffer?
        if count>=len(self.buffer): # We can use all the buffer
          data=data+self.buffer
          self.buffer=b''
        else: # We only need part of the buffer
          data=data+self.buffer[:count]
          self.buffer=self.buffer[count:]
      else: # No data in buffer, check queue
        try:
          buff=self.bufferQueue.get_nowait()
        except: # No data in queue
          print('No more data buffered')
          break
        print('adding')
        print(str(buff))
        self.buffer=self.buffer+buff # Move from queue to buffer, repeat
    self.checkClose()
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
      self.outq.put(None)
    elif state==utp.EOF:
      self.utp_socket.close()
      self.connected=False
      print('Putting None')
      self.outq.put(None)

  def on_error(self, errcode):
    print("on_error", errcode)
    print('Error, closing socket')
    if not self.closed:
      self.utp_socket.close()
    else:
      self.utp_socket=None

  def on_overhead(self, send, count, type):
    #print("on_overhead", send, count, type)
    pass
