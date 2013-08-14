import os
import sys
import time
import socket
import select
from queue import Queue, Empty
from threading import Thread

import utp.utp_socket as utp

if os.name == "nt":
  from errno import WSAEMSGSIZE as EMSGSIZE
  from errno import WSAECONNRESET as ECONNRESET
  from errno import WSAEWOULDBLOCK as EWOULDBLOCK
else:
  from errno import EMSGSIZE
  from errno import ECONNRESET
  from errno import EWOULDBLOCK

class DustUtpServerSocket(utp.Callbacks):
  def __init__(self, inq, outq, utpsock, udpsock):
    utp.Callbacks.__init__(self)
    self.inq=inq
    self.outq=outq
    self.utp_socket=utpsock
    self.udp_socket=udpsock

    self.doneIn=False
    self.doneWrite=False
    self.closed=False
    self.bufferQueue=Queue()
    self.buffer=b''
    self.connected=True
    self.connq=Queue()

    print('socket: '+str(utpsock))
    self.utp_socket.set_callbacks(self)
#    self.utp_socket.init_outgoing(self.send_to, ("127.0.0.1", 9000))

  def start(self):
    t=Thread(target=self.run)
    t.setDaemon(True)
    t.start()

  def run(self):
    while self.utp_socket:
      try:
        if self.connected and not self.doneIn:
          self.processInQueue()
        if self.doneWrite and not self.closed:
          print('closing...')
          self.closed=True
          self.utp_socket.close()
      except:
        return

  def send_to(self, data, addr):
    print('send_to: '+str(len(data)))
#    data = self.dustPacket(addr, data)
    try:
      self.udp_socket.sendto(data, 0, addr)
    except socket.error as se:
      print('socket error: '+str(se))

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
