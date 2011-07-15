import os
import re
import gc
import sys
import bz2
import time
import struct
import json
import socket
from threading import Thread, Event
from queue import Queue, Empty

from dust.crypto.keys import KeyManager
from dust.crypto.curve import Key
from dust.core.util import getPublicIP, encode, decode, encodeAddress
from dust.core.data_packet import DataPacket
from dust.server.router import PacketRouter
from dust.services.socks.proxy_packet import ProxyMessage

reqlogfile=open('requestlog.txt', 'a+b')
resplogfile=open('responselog.txt', 'a+b')

class SocksDustServer:
  def __init__(self, router, proxyAddr):
    self.router=router
    self.proxyAddr=proxyAddr
    self.lastid=0

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
        print('Accepted!')
        conn.settimeout(1)
        inq=Queue()
        outq=Queue()
        reqid=self.generateId()
        server=SocksDustHandler(conn, outq, inq)
        print('s1')
        proxy=SocksProxyHandler(reqid, self.router, self.proxyAddr, inq, outq)
        print('s2')
        server.start()
        proxy.start()
        print('starting')
        conn=None
      except:
        return

  def generateId(self):
    t=int(round(time.time()*1000000))
    return struct.pack("Q", t)[0:4]

class SocksDustHandler:
  def __init__(self, sock, inq, outq):
    print('handler')
    self.sock=sock
    self.router=router
    self.proxyAddr=proxyAddr
    self.inq=inq
    self.outq=outq
    self.closed=False
    self.wrote=0
    self.seq=0

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
      print('Got reponse data!!!')
      while data!=None:
        self.sock.sendall(data)
        self.wrote=self.wrote+len(data)
        data=self.inq.get()
      print('Closing socket! '+str(self.sock))
      print('Wrote: '+str(self.wrote))
      time.sleep(1)
      self.sock.close()
      self.sock=None
      gc.collect()
      print('Closed socket')
    except:
      if self.sock:
        self.sock.close()
      self.sock=None

  def processOut(self):
    print('reading')
    self.readHandshake()
    self.sendHandshake()
    addr, port=self.readRequest()
    self.sendResponse(addr, port)
    self.pump()

  def readHandshake(self):
    print('readHanshake')
    version=self.sock.recv(1)
    nauth=self.sock.recv(1)
    cauth=self.sock.recv(1)

  def sendHandshake(self):
    print('sendHandshake')
    self.sock.sendall(b"\x05")
    self.sock.sendall(b"\x00")

  def readRequest(self):
    print('readRequest')
    version=self.sock.recv(1)
    command=self.sock.recv(1)
    reserved=self.sock.recv(1)
    addrtype=self.sock.recv(1)
    addr=self.sock.recv(4)
    port=self.sock.recv(2)

    self.outq.put(addr+port)

    return (addr, port)

  def sendResponse(self, addr, port):
    print('sendReply')
    self.sock.sendall(b"\x05")
    self.sock.sendall(b"\x00")
    self.sock.sendall(b"\x00")
    self.sock.sendall(b"\x01")
    self.sock.sendall(addr)
    self.sock.sendall(port)

  def pump(self):
    print('pump')
    maxlen=1024
    buff=b''
    c=self.sock.recv(1)
    try:
      while c!=None and len(buff)<maxlen:
        buff=buff+c
        c=self.sock.recv(1)
      print('sending:')
      print(buff)
      self.outq.put(buff)
    except:
      self.outq.put(buff)
      self.outq.put(None)

class SocksProxyHandler:
  def __init__(self, reqid, router, proxyAddr, inq, outq):
    print('proxy')
    self.reqid=reqid
    self.router=router
    self.proxyAddr=proxyAddr
    self.inq=inq
    self.outq=outq
    self.closed=False
    self.seq=0
    self.fin=False

    self.respq=Queue()
    print('pb1')
    try:
      proxyback=self.router.getService('socksProxyback')
      print('pb2')
      proxyback.setQueue(self.reqid, self.respq)
    except Exception as e:
      print('Exception: '+str(e))
    print('pb3')
    print('proxy init done')

  def start(self):
    print('start')
    t=Thread(target=self.processIn)
    t.setDaemon(True)
    t.start()

    t2=Thread(target=self.processOut)
    t2.setDaemon(True)
    t2.start()

  def processIn(self):
    print('processIn')
    data=self.inq.get()
    while data!=None:
      packet=ProxyMessage()
      packet.createProxyMessage(self.reqid, self.seq, self.fin, data)
      router.sendto(packet.msg, proxyAddr, service='socksProxy')
      self.seq=self.seq+1
      data=self.inq.get()

    self.fin=True
    packet=ProxyMessage()
    packet.createProxyMessage(self.reqid, self.seq, self.fin, b'')
    router.sendto(packet.msg, proxyAddr, service='socksProxy')

  def processOut(self):
    print('processOut')
    data=self.respq.get()
    while data!=None:
      self.outq.put(data)
      data=self.respq.get()
    self.outq.put(None)

if __name__=='__main__':
#  passwd=sys.argv[1]
#  inport=int(sys.argv[2])
#  dest=sys.argv[3]
#  outport=int(sys.argv[4])
#  ipv=int(sys.argv[5])
#  if ipv==6:
#    v6=True
#  else:
#    v6=False

  passwd='test'
  inport=7001
#  proxyAddr='udp://[::]:9000'
  proxyAddr=('99.55.140.166', 7000)
  outport=7000
  v6=False

  host=getPublicIP(v6)
  print('Host: '+str(host))

  keys=KeyManager()
  keys.setInvitePassword(passwd)
  keys.loadKnownHosts('config/knownhosts.yaml')
  keys.loadKeypair('config/id.yaml')
  keys.loadIncomingInvites('config/incoming_invites.ip')
  keys.loadOutgoingInvites('config/outgoing_invites.ip')

  keypair=keys.getKeypair()
  pubkey=keypair.public
  destpubkey=pubkey
  pubkeyhex=encode(pubkey.bytes)
  destpubkeyhex=pubkeyhex

  router=PacketRouter(v6, inport, keys, passwd)
#  router.connect(dest, outport)

  router.start()
  server=SocksDustServer(router, proxyAddr)
#  server.start()

  while True:
    try:
      time.sleep(1)
    except:
      sys.exit(0)
