from dust.crypto.dustUtil import hash
from dust.core.dust_socket2 import dust_socket
from dust.core.util import encodeAddress, xor, encode
from dust.core.dust_packet2 import makeIV
from dust.crypto.keys import KeyManager
from dust.crypto.curve import Key
from dust.crypto.dustUtil import DustCipher

import monocle
from monocle import _o, Return

def makeSession(myAddress, address):
  addressKey=address[0]

  h1=hash(addressKey.encode('ascii'))
  h2=hash(myAddress[0].encode('ascii'))

  sessionKey=xor(h1, h2)
  return sessionKey

def makeEphemeralSession(keypair, pub):
  pub=Key(pub, False)
  return keypair.createSession(pub)

def createEphemeralKeypair():
  return KeyManager().createKeypair()

class lite_socket(object):
  def __init__(self, key):
    self.keys=KeyManager()
    self.key=key
    self.cipherIn=DustCipher(key, "\x00")
    self.cipherOut=DustCipher(key, "\x00")

  @_o
  def encrypt(self, data):
    data=self.cipherOut.encrypt(data)
    yield Return(data)

  @_o
  def decrypt(self, data):
    data=self.cipherIn.decrypt(data)
    yield Return(data)

  def createEphemeralKeypair(self):
    return self.keys.createKeypair()
