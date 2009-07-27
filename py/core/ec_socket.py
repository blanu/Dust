import sys
import time
import struct
from socket import *

import yaml

from crypto.curve import *
from core.ec_packet import DataPacket
from core.util import encodeAddress
from intro.intro import Introducer

class ec_socket:
  def __init__(self, keys):
    self.keys=keys
    self.keypair=keys.getKeypair()
    self.sock=socket(AF_INET6, SOCK_DGRAM)
    self.dest=None
    self.connectDest=None
    self.connectSessionKey=None
    self.sessionKeys={}    
        
  def setblocking(self, mode):
    self.sock.setblocking(mode)
    
  def bind(self, address):
    self.sock.bind(address)
    myaddrKey=encodeAddress(address)
    self.introducer=Introducer(self.keys, myaddrKey)    
    
  def connect(self, address):
    if address==self.connectDest:
      return

    self.connectSessionKey=self.makeSession(address, True)
    if self.connectSessionKey:
      self.connectDest=address
    else:
      self.connectDest=None
    
  def makeSession(self, address, tryInvite):    
    addressKey=encodeAddress(address)
    
    if addressKey in self.sessionKeys:
      return self.sessionKeys[addressKey]    

    if self.keys.isKnown(addressKey):
      sessionKey=self.keys.getSessionKeyForAddress(addressKey)
      self.sessionKeys[addressKey]=sessionKey
      print('SessionKey:', len(self.sessionKeys[addressKey]))
      return sessionKey
    else:
      if self.introducer and tryInvite:
        print('Unknown address', addressKey, 'trying introduction...')
        sessionKey=self.introducer.makeIntroduction(address, self.sock)
        if not sessionKey:
          print('Introduction failed.')
          return
        else:
          return sessionKey
      else:
        print('Failed to connect, no introducer (or tryInvite=False) and unknown address')
        return
      
  def recv(self, bufsize):
    if not self.connectDest or not self.connectSessionKey:
      print('Not connected')
      return None
    else:
      data, addr=self.recvfrom(bufsize)
      checkaddr=addr[0:2]
      if checkaddr!=self.connectDest:
        print('Rejecting packet from', checkaddr, 'should be from', self.connectDest)
        return None
      else:
        return data
        
  def recvfrom(self, bufsize):
    data, addr=self.sock.recvfrom(bufsize)
    if not data:
      print('No data')
      return None, None
      
    sessionKey=self.makeSession(addr, False) # Don't use an invite when you receive a packet from an unknown host, that's not the protocol
    if not sessionKey:
      print('Unknown address', addr)
      if self.introducer:
        self.introducer.acceptIntroduction(data, addr)
      return None, None
      
    packet=DataPacket()
    packet.decodeDataPacket(sessionKey, data)
    print('checking:', packet.checkMac(), ',', packet.checkTimestamp())
    if packet.checkMac() and packet.checkTimestamp():
      return packet.data, addr
      
  def send(self, data):
    if not self.connectDest or not self.connectSessionKey:
      print('send: Not connected')
      return
    else:
      self.sendto(data, self.connectDest)
      
  def sendraw(self, data):
    if not self.connectDest or not self.connectSessionKey:
      print('Not connected')
      return
    else:
      self.sendtoraw(data, self.connectDest)

  def sendto(self, data, addr):
    sessionKey=self.makeSession(addr, True)
    if not sessionKey:
      print('Unknown address', addr, 'trying introduction...')
      sessionKey=self.introducer.makeIntroduction(addr)
      if not sessionKey:
        print('Introduction failed.')
        return
    print('send to', addr)
    packet=DataPacket()
    packet.createDataPacket(sessionKey, data)
    self.sock.sendto(packet.packet, 0, addr)    

  def sendtoraw(self, data, addr):
    self.sock.sendto(data, 0, addr)