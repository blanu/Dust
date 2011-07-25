from dust.crypto.dustUtil import hash
from dust.core.dust_socket2 import dust_socket
from dust.core.util import encodeAddress, xor, encode
from dust.core.dust_packet2 import makeIV
from dust.crypto.keys import KeyManager

def makeSession(self, myAddress, address):
  addressKey=address[0]

  h1=hash(addressKey.encode('ascii'))
  h2=hash(self.myAddress[0].encode('ascii'))

  sessionKey=xor(h1, h2)
  return sessionKey

def makeEphemeralSession(self, keypair, pub):
  return keypair.createSessionBytes(pub)

class lite_socket(object):
  def __init__(self, key, ivIn=None, ivOut=None):
    self.keys=KeyManager()
    self.key=key
    self.ivIn=ivIn
    self.ivOut=ivOut
    if self.ivIn:
      self.cipherIn=DustCipher(key, ivIn)
    if self.ivOut:
      self.cipherOut=DustCipher(key, ivOut)

  def setIVIn(self, iv):
    self.ivIn=iv
    self.cipherIn=DustCipher(key, ivIn)

  def setIVOut(self, iv):
    self.ivOut=iv
    self.cipherOut=DustCipher(key, ivOut)

  def encode(self, data):
    return self.cipherOut.encrypt(data)

  def decode(self, data):
    return self.cipherIn.decrypt(data)

  def createEphemeralKeypair(self):
    return self.keys.createKeypair()
