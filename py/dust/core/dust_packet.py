import sys
import time
import struct

from dust.crypto.dustUtil import encrypt, decrypt, mac
from dust.core.util import splitField, splitFields, encode

v3=(sys.version[0]=='3')

PADDING_RANGE = 32

IV_SIZE = 16
KEY_SIZE = 32
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
  if size==4:
    return struct.pack("I", l)
  elif size==2:
    return struct.pack("H", l)
  elif size==1:
    return struct.pack("B", l)
  else:
    print('Unsupported length size:', size)
    return

# 32 bytes
def makeMac(k, data):
  result=mac(k, data)
  return result

# 16 bytes
def makeIV(entropy):
  return entropy.getBytes(IV_SIZE)

# Random number of bytes 0-size
def makePadding(entropy, size):
  num=entropy.getInt(size-1)
  return entropy.getBytes(num)

class DustPacket:
  def __init__(self):
    self.key=None

    self.mac=None
    self.ciphertext=None
    self.iv=None
    self.encrypted=None
    self.payload=None

    self.timestamp=None
    self.dataLength=None
    self.paddingLength=None
    self.data=None

    self.padding=None

    self.packet=None

    self.remaining=None

  def __str__(self):
    s="[\n"

    if self.key:
      s=s+"  key: "+encode(self.key)+"\n"
    else:
      s=s+"  key: None\n"

    if self.checkMac():
      s=s+'  MAC: '+encode(self.mac)+" OK\n"
    else:
      s=s+'  MAC: '+encode(self.mac)+" Failed\n"

    s=s+'  IV: '+encode(self.iv)+"\n"

    s=s+"  payload: "+encode(self.payload)+"\n"

    if self.checkTimestamp():
      s=s+'  timestamp: '+str(self.timestamp)+" OK\n"
    else:
      s=s+'  timestamp: '+str(self.timestamp)+" Failed\n"

    s=s+'  dataLength: '+str(self.dataLength)+"\n"
    s=s+'  paddingLength: '+str(self.paddingLength)+"\n"
    s=s+'  data: '+str(encode(self.data))+"\n"
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

  def createDustPacket(self, key, data, entropy):
    self.key=key
    if v3 and type(data)!=bytes:
      self.data=bytes(data, 'ascii')
    else:
      self.data=data

    self.padding=makePadding(entropy, PADDING_RANGE)
    payloadLength=TIMESTAMP_SIZE+DATA_LENGTH_SIZE+PADDING_LENGTH_SIZE+len(self.data)

    self.timestamp=getTime()
    timestamp=makeTimestamp(self.timestamp)
    self.dataLength=len(self.data)
    dataLength=makeLength(self.dataLength, DATA_LENGTH_SIZE)
    self.paddingLength=len(self.padding)
    paddingLength=makeLength(self.paddingLength, PADDING_LENGTH_SIZE)
    self.payload=timestamp+dataLength+paddingLength+self.data

    self.iv=makeIV(entropy)
    self.encrypted=encrypt(self.key, self.iv, self.payload)

    self.ciphertext=self.iv+self.encrypted

    self.mac=makeMac(self.key, self.ciphertext)

    self.packet=self.mac+self.ciphertext+self.padding

  def decodeDustPacket(self, key, packet):
    self.key=key
    self.packet=packet

    self.mac, self.ciphertext=splitField(self.packet, MAC_SIZE)
    self.iv, self.encrypted=splitField(self.ciphertext, IV_SIZE)
    self.payload=decrypt(self.key, self.iv, self.encrypted)

    self.timestamp, self.dataLength, self.paddingLength, self.data=splitFields(self.payload, [TIMESTAMP_SIZE, DATA_LENGTH_SIZE, PADDING_LENGTH_SIZE])
    self.timestamp=struct.unpack("I", self.timestamp)[0]
    self.dataLength=struct.unpack("H", self.dataLength)[0]
    self.paddingLength=struct.unpack("B", self.paddingLength)[0]

    self.data, extra=splitField(self.data, self.dataLength)

    payloadLength=TIMESTAMP_SIZE+DATA_LENGTH_SIZE+PADDING_LENGTH_SIZE+len(self.data)
    self.payload=self.payload[:payloadLength]

    ciphertextLength=IV_SIZE+payloadLength
    self.ciphertext=self.ciphertext[:ciphertextLength]

    realPacketLength=MAC_SIZE+ciphertextLength+self.paddingLength
    if len(packet)>realPacketLength:
      self.remaining=packet[realPacketLength:]

  def checkMac(self):
    return self.mac and self.mac==makeMac(self.key, self.ciphertext)

  def checkTimestamp(self):
    now=int(round(time.time()))
    delta=now-self.timestamp
    return delta<10*1000 # 10 secondsy

