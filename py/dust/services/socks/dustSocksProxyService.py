import bz2
import math
import time
import socket
import base64
import traceback
from dust.core.util import encodeAddress, uncompact
from dust.services.socks.proxyback_packet import ProxybackMessage
from dust.services.socks.proxy_packet import ProxyMessage

from threading import Thread, Event
from queue import Queue, Empty

class DustSocksProxyService:
  def __init__(self):
    self.router=None
    self.conns={}

  def setRouter(self, r):
    self.router=r;

  def handle(self, msock, msg, addr):
    packet=ProxyMessage()
    packet.decodeProxyMessage(msg)

    print('Socks Proxy message from '+encodeAddress(addr)+':')
    print(packet)

    reqid=packet.reqid
    seq=packet.seq
    data=packet.data

    if seq==0:
      host, port=self.readRequest(data)
      conn=TcpProxyHandler(reqid, host, port, self, addr)
      conn.start()
      self.conns[reqid]=conn
    else:
      conn=self.conns[reqid]
      conn.inq.put(data)

  def readRequest(self, data):
    return uncompact(data)

class TcpProxyHandler:
  def __init__(self, reqid, host, port, server, addr):
    self.reqid=reqid
    self.server=server
    self.addr=addr
    self.inq=Queue()
    self.seq=0
    self.fin=False
    self.doneWrite=False

    try:
      print('connecting to '+str(host)+':'+str(port))
      self.conn=socket.create_connection((host, port))
      self.conn.setblocking(0)
    except Exception as e:
      print(e)
      traceback.print_exc()

  def start(self):
    t=Thread(target=self.processIn)
    t.setDaemon(True)
    t.start()

    t2=Thread(target=self.processOut)
    t2.setDaemon(True)
    t2.start()

  def processIn(self):
    data=self.inq.get()
    while data!=None:
      self.conn.sendall(data)
      data=self.inq.get()
    self.doneWrite=True
    print('done reading from client')

  def processOut(self):
    chunkSize=512
    read=0
    buff=b''

    while not self.fin:
      print('waiting for server bytes')
      try:
        buff=self.conn.recv(chunkSize)
      except:
        print('timeout or error talking to server')
        if not self.doneWrite:
          time.sleep(1)
          continue
        else:
          buff=None
      if buff!=None:
        read=read+len(buff)
      else:
        buff=b''
        self.fin=True
        self.conn.close()

      print('sending '+str(self.seq))
      packet=ProxybackMessage()
      packet.createProxybackMessage(self.reqid, self.seq, self.fin, buff)
      print(packet)
      self.server.router.sendto(packet.msg, self.addr, service='socksProxyback')
      # FIXME - remove conn from self.server.conns on close

    print('done talking to server')
