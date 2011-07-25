from dust.crypto.dustUtil import hash
from dust.core.dust_socket import dust_socket2
from dust.core.util import encodeAddress, xor, encode
from dust.core.dust_packet import makeIV
from dust.crypto.keys import KeyManager

def makeSession(self, myAddress, address):
  addressKey=address[0]

  h1=hash(addressKey.encode('ascii'))
  h2=hash(self.myAddress[0].encode('ascii'))

  sessionKey=xor(h1, h2)
  return sessionKey

class lite_socket(object):
  def __init__(self, key, ivIn=None):
    self.keys=KeyManager()
    self.key=key
    self.ivIn=ivIn
    self.ivOut=ivOut
    if self.ivIn:
      self.cipherIn=DustCipher(key, ivIn)
    self.cipherOut=DustCipher(key, ivOut)

  def setIV(self, iv):
    self.ivIn=iv
    self.cipherIn=DustCipher(key, ivIn)

  def encode(self, data):
    return self.cipherOut.encrypt(data)

  def decode(self, data):
    return self.cipherIn.decrypt(data)
