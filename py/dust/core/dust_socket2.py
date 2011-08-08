import sys
import time
import struct
from socket import *

import yaml

from dust.crypto.curve import *
from dust.core.data_packet2 import DataPacket
from dust.core.util import encodeAddress, encode
from dust.intro.intro import Introducer

debug=False

class dust_socket:
  def __init__(self, keys, socket=None):
    self.keys=keys
    if keys:
      self.keypair=keys.getKeypair()

    self.myAddress=None
    self.myAddressKey=None

    self.dest=None
    self.connectDest=None
    self.connectSessionKey=None
    self.sessionKeys={}

    self.remaining=None

    if socket:
      self.sock=socket
      address=self.sock.getsockname()
      self.setAddress(address)

  def setIV(self, iv):
    self.iv=iv

  def setAddress(self, address):
    self.introducer=Introducer(self.keys, address)
    self.myAddress=address
    self.myAddressKey=encodeAddress(address)

  def bind(self, address):
    ip=address[0]
    if ':' in ip:
      print('Binding to v6: '+str(address))
      self.sock=socket(AF_INET6, SOCK_DGRAM)
    else:
      print('Binding to v4: '+str(address))
      self.sock=socket(AF_INET, SOCK_DGRAM)
    self.sock.bind(address)
    self.setAddress(address)

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
    if self.remaining:
      data, addr=self.remaining
      self.remaining=None
    else:
      data, addr=self.sock.recvfrom(bufsize)

    if not data:
      print('Dust: No data')
      return None, None
    else:
      packet=self.decodePacket(addr, data)
      if not packet:
        print('Dust: No packet')
        return None, None
      else:
        if debug:
          print('Received: '+str(packet))
        if packet.remaining:
          self.remaining=(packet.remaining, addr)
        if type(packet)==DataPacket:
          return packet.data, addr
        else:
          print('Not a data packet')
          return None, None

  def decodePacket(self, addr, data):
    sessionKey=self.makeSession(addr, False) # Don't introduce yourself when you receive a packet from an unknown host, that's not the protocol
    if sessionKey: # Must be a data packet
      packet=DataPacket()
      packet.decodeDataPacket(sessionKey, data)
      if packet.checkMac() and packet.checkTimestamp():
        return packet
      else:
        print('Integrity failed', packet.checkMac(), packet.checkTimestamp())
        return None
    else: # Must be an intro packet
      print('Unknown address', addr)
      if self.introducer:
        intro=self.introducer.acceptIntroduction(data, addr)
        if intro:
          return intro
        else:
          return None

  def encodePacket(self, addr, data):
    sessionKey=self.makeSession(addr, True)
    if not sessionKey:
      print('Unknown address', addr, 'trying introduction...')
      sessionKey=self.introducer.makeIntroduction(addr, self.sock)
      if not sessionKey:
        print('Introduction failed.')
        return
    packet=DataPacket()
    packet.createDataPacket(sessionKey, data, self.keys.entropy)
    return packet

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
    packet=self.encodePacket(addr, data)
    if debug:
      print('Sending '+str(packet))
    self.sock.sendto(packet.packet, 0, addr)

  def sendtoraw(self, data, addr):
    self.sock.sendto(data, 0, addr)
