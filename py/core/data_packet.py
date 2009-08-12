import time
import struct

from skein import threefish, skein512
from crypto.curve import *
from crypto.skeinUtil import encrypt, decrypt

from core.util import splitField, splitFields, encode

IV_SIZE = 16
KEY_SIZE = 32
BLOCK_SIZE = 32
MAC_SIZE = 32
TIMESTAMP_SIZE=4
DATA_LENGTH_SIZE=2
PADDING_LENGTH_SIZE=1

HEADER_SIZE=IV_SIZE+MAC_SIZE+TIMESTAMP_SIZE+DATA_LENGTH_SIZE+PADDING_LENGTH_SIZE

# 4 bytes
def getTime():
  t=int(round(time.time()))
  return t

def makeTimestamp(t):
  return struct.pack("I", t)

# 4 bytes
def makeLength(l, size):
  if size==2:
    return struct.pack("H", l)
  elif size==1:
    return struct.pack("B", l)
  else:
    print('Unsupported length size:', size)
    return

# 32 bytes
def makeMac(k, data):
  result=skein512(data, digest_bits=256, mac=k).digest()
  return result

# From 0 to size bytes
def makeFiller(size, l):
  if l<size:
    padl=size-l
  elif l%size==0:
    return b''
  else:
    padl=size-(l % size)
  return b"\x00"*padl    

# 16 bytes
def makeIV(entropy):
  return entropy.getBytes(IV_SIZE)

# Random number of bytes 0-size
def makePadding(entropy, size):
  num=entropy.getInt(size-1)
  return entropy.getBytes(num)
      
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
    
    self.remaining=None
    
  def __str__(self):
    s="[\n"
    s=s+'  IV: '+encode(self.iv)+"\n"
    
    if self.checkMac():
      s=s+'  MAC: '+encode(self.mac)+" OK\n"
    else:
      s=s+'  MAC: '+encode(self.mac)+" Failed\n"
      
    if self.checkTimestamp():
      s=s+'  timestamp: '+str(self.timestamp)+" OK\n"
    else:
      s=s+'  timestamp: '+str(self.timestamp)+" Failed\n"
      
    s=s+'  dataLength: '+str(self.dataLength)+"\n"
    s=s+'  paddingLength: '+str(self.paddingLength)+"\n"
    s=s+'  data: '+str(self.data)+"\n"
    if self.filler:
      s=s+'  filler: '+str(len(self.filler))+"\n"
    else:
      s=s+"  filler: None\n"
    if self.padding:
      s=s+'  padding: '+encode(self.padding)+"\n"
    else:
      s=s+"  padding: None\n"
    if self.remaining:
      s=s+'  remaining: '+encode(self.remaining)+"\n"
    else:
      s=s+"  remaining: None\n"
    s=s+"]\n"
    return s
    
  def createDataPacket(self, sk, data, entropy):
    self.sk=sk
    self.data=data

    self.padding=makePadding(entropy, BLOCK_SIZE)    
    bodyLength=TIMESTAMP_SIZE+DATA_LENGTH_SIZE+PADDING_LENGTH_SIZE+len(self.data)
    self.filler=makeFiller(BLOCK_SIZE, MAC_SIZE+bodyLength)
    
    self.timestamp=getTime()
    timestamp=makeTimestamp(self.timestamp)
    self.dataLength=len(self.data)
    dataLength=makeLength(self.dataLength, DATA_LENGTH_SIZE)
    self.paddingLength=len(self.padding)
    paddingLength=makeLength(self.paddingLength, PADDING_LENGTH_SIZE)
    self.body=timestamp+dataLength+paddingLength+self.data
    
    self.mac=makeMac(self.sk, self.body)
    self.payload=self.mac+self.body+self.filler
  
    self.iv=makeIV(entropy)
    self.encrypted=encrypt(self.sk, self.iv, self.payload)
  
    self.packet=self.iv+self.encrypted+self.padding
    
  def decodeDataPacket(self, sk, packet):
    self.sk=sk
    self.packet=packet

    self.iv, self.encrypted=splitField(self.packet, IV_SIZE)
    r=len(self.encrypted) % BLOCK_SIZE
    if r>0:
      self.encrypted=self.encrypted[:-r]
    self.payload=decrypt(self.sk, self.iv, self.encrypted)

    self.mac, self.body=splitField(self.payload, MAC_SIZE)

    self.timestamp, self.dataLength, self.paddingLength, self.data=splitFields(self.body, [TIMESTAMP_SIZE, DATA_LENGTH_SIZE, PADDING_LENGTH_SIZE])
    self.timestamp=struct.unpack("I", self.timestamp)[0]
    self.dataLength=struct.unpack("H", self.dataLength)[0]
    self.paddingLength=struct.unpack("B", self.paddingLength)[0]

    
    self.data, extra=splitField(self.data, self.dataLength)
    
    bodyLength=TIMESTAMP_SIZE+DATA_LENGTH_SIZE+PADDING_LENGTH_SIZE+len(self.data)
    self.body=self.body[:bodyLength]

    payloadLength=MAC_SIZE+bodyLength
    payloadLength=payloadLength+len(makeFiller(BLOCK_SIZE, payloadLength))
    
    realPacketLength=IV_SIZE+payloadLength+self.paddingLength
    if len(packet)>realPacketLength:
      self.remaining=packet[realPacketLength:]
      
  def checkMac(self):
    return self.mac and self.mac==makeMac(self.sk, self.body)
    
  def checkTimestamp(self):
    now=int(round(time.time()))
    delta=now-self.timestamp
    return delta<10
        
