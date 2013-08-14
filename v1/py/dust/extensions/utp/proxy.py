import os
import gc
import sys
import time
import socket
from threading import Thread, Event
from queue import Queue, Empty
from subprocess import Popen, PIPE

from dust.extensions.utp.dustUtpSocket import DustUtpSocket

class TcpServer:
  def __init__(self):
    host='0.0.0.0'
    port=9991
    print('Listening on '+host+':'+str(port))

    self.sock=socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
#    self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
    bound=False
    while not bound:
      try:
        self.sock.bind((host, port))
        bound=True
      except:
        print('Error binding to port '+str(port))
        time.sleep(30)

    self.sock.listen(5)
    while(True):
      try:
        conn, addr = self.sock.accept()
        conn.settimeout(1)
        inq=Queue()
        outq=Queue()
        dus=DustUtpSocket(inq, outq)
        proxy=TcpProxy(conn, outq, inq)
        conn=None
        proxy.start()
        dus.start()
      except:
        return

class TcpProxy:
  def __init__(self, sock, inq, outq):
    self.sock=sock
    self.inq=inq
    self.outq=outq
    self.closed=False

  def start(self):
    t=Thread(target=self.processIn)
    t.setDaemon(True)
    t.start()

    t2=Thread(target=self.processOut)
    t2.setDaemon(True)
    t2.start()

  def processIn(self):
    try:
      data=self.inq.get()
      while data:
        print('data: '+str(data)+' '+str(self.sock))
        self.sock.sendall(data)
        data=self.inq.get()
      print('Closing socket! '+str(self.sock))
      self.sock.close()
      self.sock=None
      gc.collect()
      print('Closed socket')
    except:
      if self.sock:
        self.sock.close()
      self.sock=None

  def processOut(self):
    data=self.sock.recv(1024)
    while self.sock and data:
      self.outq.put(data)
      try:
        data=self.sock.recv(1024)
      except:
        if self.sock:
          self.sock.close()
        self.sock=None
        gc.collect()
        return

if __name__=='__main__':
  server=TcpServer()
