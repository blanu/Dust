import sys
import time
import struct

from dust.crypto.dustUtil import encrypt, decrypt, mac
from dust.core.util import splitField, splitFields, encode

v3=(sys.version[0]=='3')

IV_SIZE = 16
KEY_SIZE = 32

# 16 bytes
def makeIV(entropy):
  return entropy.getBytes(IV_SIZE)

class DustPacket:
  def __init__(self):
    self.key=None

    self.ciphertext=None
    self.iv=None
    self.encrypted=None
    self.payload=None

    self.data=None

    self.packet=None

    self.remaining=None

  def __str__(self):
    s="[\n"

    if self.key:
      s=s+"  key: "+encode(self.key)+"\n"
    else:
      s=s+"  key: None\n"

    s=s+'  IV: '+encode(self.iv)+"\n"

    s=s+"  payload: "+encode(self.payload)+"\n"

    s=s+'  dataLength: '+str(self.dataLength)+"\n"
    s=s+'  data: '+str(encode(self.data))+"\n"
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

    payloadLength=len(self.data)

    self.dataLength=len(self.data)
    self.payload=self.data

    self.encrypted=encrypt(self.key, self.iv, self.payload)

    self.ciphertext=self.encrypted

    self.packet=self.ciphertext

  def decodeDustPacket(self, key, packet):
    self.key=key
    self.packet=packet

    self.ciphertext=self.payload
    self.encrypted=self.ciphertext
    self.payload=decrypt(self.key, self.iv, self.encrypted)

    self.data=self.payload

    self.ciphertext=self.data

