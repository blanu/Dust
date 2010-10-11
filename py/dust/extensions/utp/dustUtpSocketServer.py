import os
import sys
import time
import socket
import select
from threading import Thread
from queue import Queue, Empty

import utp.utp_socket as utp
from dust.crypto.keys import KeyManager
from dust.extensions.lite.lite_socket import lite_socket
from dust.extensions.utp.dustUtpServerSocket import DustUtpServerSocket

if os.name == "nt":
  from errno import WSAEMSGSIZE as EMSGSIZE
  from errno import WSAECONNRESET as ECONNRESET
  from errno import WSAEWOULDBLOCK as EWOULDBLOCK
else:
  from errno import EMSGSIZE
  from errno import ECONNRESET
  from errno import EWOULDBLOCK

class DustUtpSocketServer:
  def __init__(self):
    self.connq=Queue()

    self.udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    self.udp_socket.bind(('127.0.0.1', 9000))
    self.udp_socket.setblocking(0)

    size = 2 * 1024 * 1024
    self.udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, size)
    self.udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_SNDBUF, size)

    self.dust = lite_socket(KeyManager())
    self.dust.setAddress(self.udp_socket.getsockname())
    print(self.udp_socket.getsockname())

    self.utp_socket = utp.Socket()
    self.utp_socket.init_outgoing(self.send_to, ("127.0.0.1", 9000))
    print("connecting", self.utp_socket, self.utp_socket.getpeername())

  def start(self):
    t=Thread(target=self.run)
    t.setDaemon(True)
    t.start()

  def run(self):
    while self.utp_socket:
      try:
        print('main')
        self.udp_select(0.5)
        utp.CheckTimeouts()
      except:
        print('Exception in socket server')
        break

  def accept(self, inq, outq):
    print("Waiting for connection...")
    utp_socket=self.connq.get()
#    utp_socket.init_outgoing(self.send_to, ("127.0.0.1", 9000))
    print('Got connection: '+str(utp_socket))
    serverSock=DustUtpServerSocket(inq, outq, utp_socket, self.udp_socket)
    return serverSock

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

  def got_incoming_connection(self, utp_socket):
    print("got_incoming_connection!", utp_socket, utp_socket.getpeername())
    self.connq.put(utp_socket)

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
        utp.IsIncomingUTP(self.got_incoming_connection, self.send_to, data, addr)
    for s in e:
      print("socket error!", s)
