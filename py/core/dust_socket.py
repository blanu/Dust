import sys
import time
import struct
from socket import *
from Queue import Queue

import yaml

from crypto.curve import *
from core.data_packet import DataPacket
from core.util import encodeAddress
from intro.intro import Introducer

class dust_socket:
  def __init__(self, keys):
    self.keys=keys
    self.keypair=keys.getKeypair()
    
    self.dest=None
    self.connectDest=None
    self.connectSessionKey=None
    self.sessionKeys={}    

    self.inbuff=Queue()
    
  def bind(self, address):
    ip=address[0]
    if ':' in ip:
      self.sock=socket(AF_INET6, SOCK_DGRAM)
    else:
      self.sock=socket(AF_INET, SOCK_DGRAM)
    self.sock.bind(address)
    self.introducer=Introducer(self.keys, address)    
    
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
    if not self.inbuff.empty():
      data, addr=self.inbuff.get()
    else:
      data, addr=self.sock.recvfrom(bufsize)
      
    if not data:
      print('No data')
      return None, None
    else:
      packet=self.decodePacket(addr, data)
      if not packet:
        return None, None
      else:
        if packet.remaining:
          self.inbuff.put((remaining, addr))
        return packet
      
  def decodePacket(self, addr, data):
    sessionKey=self.makeSession(addr, False) # Don't introduce yourself when you receive a packet from an unknown host, that's not the protocol
    if sessionKey: # Must be a data packet
      packet=DataPacket()
      packet.decodeDataPacket(sessionKey, data)
      print('checking:', packet.checkMac(), ',', packet.checkTimestamp())
      if packet.checkMac() and packet.checkTimestamp():        
        return packet.data, addr
    else: # Must be an intro packet
      print('Unknown address', addr)
      if self.introducer:
        intro=self.introducer.acceptIntroduction(data, addr)
        if intro and intro.chained=True:
        else:
          return None, None
      
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
    packet.createDataPacket(sessionKey, data, self.keys.entropy)
    self.sock.sendto(packet.packet, 0, addr)    

  def sendtoraw(self, data, addr):
    self.sock.sendto(data, 0, addr)