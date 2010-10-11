import os
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
from dust.services.http.proxy_packet import ProxyMessage

class HttpDustServer:
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
        conn.settimeout(1)
        inq=Queue()
        outq=Queue()
        reqid=self.generateId()
        server=HttpDustHandler(conn, outq, inq)
        proxy=HttpProxyHandler(reqid, self.router, self.proxyAddr, inq, outq)
        server.start()
        proxy.start()
        conn=None
      except:
        return

  def generateId(self):
    t=int(round(time.time()))
    return struct.pack("I", t)

class HttpDustHandler:
  def __init__(self, sock, inq, outq):
    self.sock=sock
    self.router=router
    self.proxyAddr=proxyAddr
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
      print('Got reponse data!!!')
      while data!=None:
#        data=bz2.compress(data)
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
    data=self.readDestination()
    self.outq.put(data)
#    self.readHeaders()
#    data=self.sock.recv(1024)
#    while self.sock and data:
#      self.outq.put(data)
#      try:
#        data=self.sock.recv(1024)
#      except:
#        if self.sock:
#          self.sock.close()
#        self.sock=None
#        gc.collect()
#        return

  def readDestination(self):
    buff=b''
    c=self.sock.recv(1)
    while c!=b"\r" and c!=b"\n":
      print('c: '+str(c))
      buff=buff+c
      c=self.sock.recv(1)
    parts=buff.decode('ascii').split(' ')
    method=parts[0]
    dest=parts[1]
    v=parts[2]
    print('Proxy request: '+str(method)+' '+str(dest)+' '+str(v))
    self.method=method
    self.dest=dest
    self.version=v
    return buff

  def readHeaders(self):
    self.headers=[]
    buff=b''
    c=self.sock.recv(1)
    while c!=b"\r" and c!=b"\n":
      print('c: '+str(c))
      buff=buff+c
      c=self.sock.recv(1)
    parts=buff.decode('ascii').split(' ')
    method=parts[0]
    dest=parts[1]
    v=parts[2]
    print('Proxy request: '+str(method)+' '+str(dest)+' '+str(v))
    self.method=method
    self.dest=dest
    self.version=v
    return buff

class HttpProxyHandler:
  def __init__(self, reqid, router, proxyAddr, inq, outq):
    self.reqid=reqid
    self.router=router
    self.proxyAddr=proxyAddr
    self.inq=inq
    self.outq=outq
    self.closed=False

    self.respq=Queue()
    proxyback=self.router.getService('httpProxyback')
    proxyback.setQueue(self.reqid, self.respq)

  def start(self):
    t=Thread(target=self.processIn)
    t.setDaemon(True)
    t.start()

    t2=Thread(target=self.processOut)
    t2.setDaemon(True)
    t2.start()

  def processIn(self):
    data=self.inq.get()
    packet=ProxyMessage()
    packet.createProxyMessage(self.reqid, data)
    router.sendto(packet.msg, proxyAddr, service='httpProxy')

  def processOut(self):
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
  proxyAddr=('2001:0:53aa:64c:14cb:442c:baa2:c211', 7000)
  outport=7000
  v6=True

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
  server=HttpDustServer(router, proxyAddr)
#  server.start()

  while True:
    try:
      time.sleep(1)
    except:
      sys.exit(0)
