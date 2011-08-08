from dust.crypto.dustUtil import hash
from dust.core.dust_socket2 import dust_socket
from dust.core.util import encodeAddress, xor, encode
from dust.core.dust_packet2 import makeIV
from dust.crypto.keys import KeyManager
from dust.crypto.curve import Key
from dust.crypto.dustUtil import DustCipher

def makeSession(myAddress, address):
  addressKey=address[0]

  h1=hash(addressKey.encode('ascii'))
  h2=hash(myAddress[0].encode('ascii'))

  sessionKey=xor(h1, h2)
  return sessionKey

def makeEphemeralSession(keypair, pub):
  pub=Key(pub, False)
  return keypair.createSession(pub)

class lite_socket(object):
  def __init__(self, key, ivIn=None, ivOut=None):
    self.keys=KeyManager()
    self.key=key
    self.ivIn=ivIn
    self.ivOut=ivOut
    if self.ivIn:
      self.cipherIn=DustCipher(key, self.ivIn)
    if not self.ivOut:
      self.ivOut=self.newIV()
    self.cipherOut=DustCipher(key, self.ivOut)

  def newIV(self):
    return makeIV(self.keys.entropy)

  def setIVIn(self, iv):
    self.ivIn=iv
    self.cipherIn=DustCipher(self.key, self.ivIn)

  def setIVOut(self, iv):
    self.ivOut=iv
    self.cipherOut=DustCipher(self.key, self.ivOut)

  def encrypt(self, data):
    data=self.cipherOut.encrypt(data)
    return data

  def decrypt(self, data):
    data=self.cipherIn.decrypt(data)
    return data

  def createEphemeralKeypair(self):
    return self.keys.createKeypair()
