import time
import struct
import random
import binascii

from core.ec_packet import DataPacket
from invite.invite import Invite, InvitePackage
from core.util import getAddress
from crypto.curve import Key, Keypair

ID_LENGTH=16
PUBKEY_LENGTH=32
PRIVKEY_LENGTH=32

# 1 byte
def makeByteLength(data):
  b=bytearray(1)
  b[0]=len(data)
  print('mbl#@#@#:', b)
  return b
  
class IntroMessage:
  def __init__(self):
    self.keypair=None
    
  def createIntroMessage(self, pubkey):
    self.pubkey=pubkey
    publicKey=self.pubkey.bytes
    self.message=publicKey
    print('message:', binascii.hexlify(self.message))
    
  def decodeIntroMessage(self, message):
    self.message=message
    print('message:', binascii.hexlify(self.message))
    publicKey=self.message
    self.pubkey=Key(publicKey, False)

class IntroPacket(DataPacket):
  def __init__(self):
    DataPacket.__init__(self)

    self.identifier=None
    self.intro=None
    
  def createIntroPacket(self, sk, id, pubkey):
    self.identifier=id
    self.intro=IntroMessage()
    self.intro.createIntroMessage(pubkey)
    
    self.createDataPacket(sk, self.intro.message)
    self.packet=self.identifier+self.packet
  
  def decodeIntroPacket(self, choices, packet):
    self.identifier=packet[:ID_LENGTH]
    packet=packet[ID_LENGTH:]
    print('choices:', choices)
    if not self.identifier in choices:
      print('Unknown invite id', binascii.hexlify(self.identifier))
      print(choices)
      return
    sk=choices[self.identifier].secret
    
    self.decodeDataPacket(sk, packet)
    self.intro=IntroMessage()
    self.intro.decodeIntroMessage(self.data)
    
if __name__=='__main__':
  from crypto.curve import *
  sender=createKeypair()
  receiver=createKeypair()
  
  print('sender:', sender)
  
  port=7000
  addressKey=getAddress(port)
  
  ip=InvitePackage()
  ip.generate(port, 1)  
  choices=ip.invites[addressKey]
  print('choices:', choices)
  i=choices.copy().popitem()[1]
  
  print('ip:', ip.serialize())
  
  packet=IntroPacket()
  packet.createIntroPacket(i.secret, i.identifier, sender.public)
#  print('packet:', packet)
  print('packetData:', binascii.hexlify(packet.packet))
#  print('length', packet.length)
#  print('checkMac:', packet.checkMac())
  print('packet length:', len(packet.packet))
  
  print('------------------------')
  
  p2=IntroPacket()
  p2.decodeIntroPacket(choices, packet.packet)
#  print('packet:', p2)
#  print('packet.data:', p2.data)
#  print('length', p2.length)
  print('checkMac:', p2.checkMac())
  print('checkTimestamp:', p2.checkTimestamp())
  print('id:', p2.identifier)
  print('intro:', p2.intro)
  print('sender:', sender)
  print('intro pubkey:', p2.intro.pubkey)
  