import os
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
    self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
    bound=False
    while not bound:
      try:
        self.sock.bind((host, port))
        bound=True
      except:
        print('Error binding to port '+str(port))
        time.sleep(30)

    self.sock.listen(1)
    while(True):
      conn, addr = self.sock.accept()
      handler=TcpSocketHandler(conn, addr)
      handler.start()

class TcpSocketHandler:
  def __init__(self, conn, addr):
    self.conn=conn
    self.addr=addr

    inq=Queue()
    outq=Queue()

    self.dus=DustUtpSocket(inq, outq)
    self.proxy=TcpProxy(conn, outq, inq)

  def start(self):
    self.proxy.start()
    self.dus.start()

class TcpProxy:
  def __init__(self, sock, inq, outq):
    self.sock=sock
    self.inq=inq
    self.outq=outq

  def start(self):
    t=Thread(target=self.processIn)
    t.setDaemon(True)
    t.start()

    t2=Thread(target=self.processOut)
    t2.setDaemon(True)
    t2.start()

  def processIn(self):
    data=self.inq.get()
    while data:
      self.sock.sendall(data)
      data=self.inq.get()
    print('Closing socket!')
    self.sock.close()

  def processOut(self):
    data=self.sock.recv(1024)
    while data:
      self.outq.put(data)
      try:
        data=self.sock.recv(1024)
      except:
        return

if __name__=='__main__':
  server=TcpServer()
