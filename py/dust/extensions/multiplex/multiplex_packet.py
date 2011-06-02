import sys
from dust.core.data_packet import DataPacket

v3=(sys.version[0]=='3')

# 1 byte
def makeByteLength(data):
  b=bytearray(1)
  b[0]=len(data)
  return bytes(b)

class MultiplexMessage:
  def __init__(self):
    self.serviceName=None
    self.serviceNameLength=None
    self.data=None
    self.message=None

  def createMultiplexMessage(self, serviceName, data):
    if v3 and type(serviceName)!=bytes:
      self.serviceName=bytes(serviceName, 'ascii')
    else:
      self.serviceName=serviceName
    self.serviceNameLength=makeByteLength(self.serviceName)
    if v3 and type(data)!=bytes:
      self.data=bytes(data, 'ascii')
    else:
      self.data=data
    self.message=self.serviceNameLength+self.serviceName+self.data

  def decodeMultiplexMessage(self, message):
#    print('decoding multiplex message')
    self.message=message
    self.serviceNameLength=self.message[0]
    serviceName=self.message[1:self.serviceNameLength+1]
    if v3:
      self.serviceName=serviceName.decode('ascii')
    else:
      self.serviceName=serviceName
    self.data=self.message[self.serviceNameLength+1:]
#    print('decoded multiplex message')

class MultiplexPacket(DataPacket):
  def __init__(self):
    DataPacket.__init__(self)

    self.multiplex=None

  def createMultiplexPacket(self, sk, serviceName, data):
    self.multiplex=MultiplexMessage()
    self.multiplex.createMultiplexMessage(serviceName, data)
    self.createDataPacket(sk, self.multiplex.message)

  def decodeMultiplexPacket(self, sk, packet):
    self.decodeDataPacket(sk, packet)
    self.multiplex=MultiplexMessage()
    self.multiplex.decodeMultiplexMessage(self.data)

if __name__=='__main__':
  from dust.crypto.curve import *
  sender=createKeypair()
  receiver=createKeypair()

  psk=sender.createSession(receiver.public).bytes
  l=len(psk)
  if l<16:
    padding=b"\x00"*(16-l)
  else:
    padding=b"\x00"*(l%16)
  psk=psk+padding
  print(psk)

  packet=MultiplexPacket()
  packet.createMultiplexPacket(psk, 'mail', b'Hello')
  print('packet:', packet)
  print('packetData:', packet.packet)
  print('length', packet.length)
  print('checkMac:', packet.checkMac())
  print('packet length:', len(packet.packet))

  print('------------------------')

  p2=MultiplexPacket()
  p2.decodeMultiplexPacket(psk, packet.packet)
  print('packet:', p2)
  print('packet.data:', p2.data)
  print('length', p2.length)
  print('checkMac:', p2.checkMac())
  print('checkTimestamp:', p2.checkTimestamp())
  print('multiplex:', p2.multiplex)
  print('multiplex service:', p2.multiplex.serviceName)
  print('multiplex data:', p2.multiplex.data)