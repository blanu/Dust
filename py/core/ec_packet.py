import time
import struct
import random

from skein import threefish, skein512
from crypto.curve import *

IV_SIZE = 16
KEY_SIZE = 32
BLOCK_SIZE = 32
MAC_SIZE = 32

# 4 bytes
def makeTimestamp():
  t=int(round(time.time()))
  return struct.pack("I", t)

# 4 bytes
def makeLength(data):
  l=len(data)
  return struct.pack("I", l)

# 32 bytes
def makeMac(k, data):
  result=skein512(data, digest_bits=256, mac=k).digest()
  return result

# From 0 to size bytes
def makeFiller(size, data):
  l=len(data)
  if l<size:
    padl=size-l
  elif l%size==0:
    return b''
  else:
    padl=size-(l % size)
  return b"\x00"*padl    

# 16 bytes
def makeIV():
  return bytes(random.randint(0, 255) for _ in range(16))

# len(payload) bytes
def encrypt(k, iv, payload):
  cipher = threefish(k, iv)
  encrypted=bytearray(len(payload))

  start=0
  while start<len(encrypted):
    end=start+BLOCK_SIZE
    encrypted[start:end] = cipher.encrypt_block(payload[start:end])
    cipher.tweak = encrypted[start:start+16]  # cipher block chaining with first 16 bytes 
    start=end
    
  return bytes(encrypted)

# len(payload) bytes
def decrypt(k, iv, payload):
  cipher = threefish(k, iv)
  decrypted=bytearray(len(payload))
  
  start=0
  while start<len(payload):
    end=start+BLOCK_SIZE
    decrypted[start:end] = cipher.decrypt_block(payload[start:end])
    cipher.tweak = payload[start:start+16]  # cipher block chaining with first 16 bytes 
    start=end
    
  return bytes(decrypted)

# Random number of bytes 0-size
def makePadding(size):
  return bytes(random.randint(0, 255) for _ in range(random.randint(0, size-1)))
      
class DataPacket:
  def __init__(self):
    self.sk=None
    
    self.timestamp=None
    self.data=None
    
    self.length=None
    self.body=None
    
    self.mac=None
    self.filler=None
    self.payload=None
    
    self.iv=None
    self.encrypted=None
    self.padding=None
    
    self.packet=None
    
  def createDataPacket(self, sk, data):
    self.sk=sk+makeFiller(KEY_SIZE, sk)
    self.data=data
    
    self.timestamp=makeTimestamp()
    self.length=makeLength(self.data)
    self.body=self.timestamp+self.length+self.data
    
    self.mac=makeMac(self.sk, self.body)
    self.filler=makeFiller(BLOCK_SIZE, self.mac+self.body)
    self.payload=self.mac+self.body+self.filler
  
    self.iv=makeIV()
    self.encrypted=encrypt(self.sk, self.iv, self.payload)
    self.padding=makePadding(BLOCK_SIZE)
  
    self.packet=self.iv+self.encrypted+self.padding
  
  def decodeDataPacket(self, sk, packet):
    self.sk=sk+makeFiller(KEY_SIZE, sk)
    self.packet=packet
    
    self.iv=self.packet[:IV_SIZE]
    self.encrypted=self.packet[IV_SIZE:]
    l=len(self.encrypted)
    tail=l%BLOCK_SIZE
    if l>BLOCK_SIZE and tail!=0:
      self.encrypted=self.encrypted[:-tail]
    self.payload=decrypt(self.sk, self.iv, self.encrypted)

    self.mac=self.payload[:MAC_SIZE]
    self.body=self.payload[MAC_SIZE:]
  
    self.timestamp=self.body[:4]
    self.timestampValue=struct.unpack("I", self.timestamp)[0]
    self.length=self.body[4:8]
    self.lengthValue=struct.unpack("I", self.length)[0]
    self.body=self.body[:8+self.lengthValue]
    self.data=self.body[8:]
    
  def checkMac(self):
    return self.mac and self.mac==makeMac(self.sk, self.body)
    
  def checkTimestamp(self):
    now=int(round(time.time()))
    delta=now-self.timestampValue
    return delta<10
    
if __name__=='__main__':
  sender=createKeypair()
  receiver=createKeypair()
  
  psk=sender.createSession(receiver.public).bytes
  l=len(psk)
  if l<16:
    padding=b"\x00"*(16-l)
  else:    
    padding=b"\x00"*(l%16)
  psk=psk+padding
  
  packet=DataPacket()
  packet.createDataPacket(psk, b"Hello")
  print('packet:', packet)
  print('packetData:', packet.packet)
  print('length', packet.length)
  print('checkMac:', packet.checkMac())
  print('packet length:', len(packet.packet))
  
  print('------------------------')
  
  p2=DataPacket()
  p2.decodeDataPacket(psk, packet.packet)
  print('packet:', p2)
  print('packet.data:', p2.data)
  print('length', p2.length)
  print('checkMac:', p2.checkMac())
  print('checkTimestamp:', p2.checkTimestamp())
  time.sleep(1)
  print('checkTimestamp:', p2.checkTimestamp())
  time.sleep(10)
  print('checkTimestamp:', p2.checkTimestamp())