from core.data_packet import DataPacket

# 1 byte
def makeByteLength(data):
  b=bytearray(1)
  b[0]=len(data)
  return b
  
class MultiplexMessage:
  def __init__(self):
    self.serviceName=None
    self.serviceNameLength=None
    self.data=None
    self.message=None
    
  def createMultiplexMessage(self, serviceName, data):
    self.serviceName=serviceName.encode('ascii')
    self.serviceNameLength=makeByteLength(self.serviceName)
    self.data=data
    self.message=self.serviceNameLength+self.serviceName+self.data
    
  def decodeMultiplexMessage(self, message):
    self.message=message
    self.serviceNameLength=self.message[0]
    self.serviceName=self.message[1:self.serviceNameLength+1].decode('ascii')
    self.data=self.message[self.serviceNameLength+1:]

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
  from crypto.curve import *
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