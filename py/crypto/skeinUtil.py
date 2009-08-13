import random
import struct

from skein import skein512
from core.util import splitFields, splitField

SEED_SIZE=16
IV_SIZE=16
BLOCK_SIZE=32

def pbkdf(pb, salt, i, pers=None, digest_bits=256):
  data=(pb.encode('ascii')+salt)*i
  if pers:
    return skein512(data, pers=pers, digest_bits=digest_bits).digest()
  else:
    return skein512(data, digest_bits=digest_bits).digest()
  
class SkeinPRNG:
  def __init__(self, seed=None):
    if seed:
      self.seed=seed
    else:
      self.seed=self.generateSeed()
      
  def generateSeed(self):
    return bytes(random.randint(0, 255) for _ in range(SEED_SIZE))

  def reseed(self, seed):
    self.seed=skein512(self.seed+seed, digest_bits=SEED_SIZE*8).digest()
    
  def getBytes(self, n):
    result=skein512(self.seed, digest_bits=(SEED_SIZE+n)*8).digest()
    self.seed, r=splitFields(result, [SEED_SIZE, n])
    return r

  def getInt(self, max=None):
    bs=self.getBytes(4)
    i=struct.unpack('I', bs)[0]
    if max:      
      return i%max
    else:
      return i
    
def encrypt(k, iv, data):
  cipher=SkeinCipher(k, iv)
  return cipher.encrypt(data)
      
def decrypt(k, iv, data):
  cipher=SkeinCipher(k, iv)
  return cipher.decrypt(data)
  
class SkeinCipher:
  def __init__(self, key, iv, pers=None):
    self.key=key
    self.iv=iv
    self.entropy=b''
    self.pers=pers

  def getBytes(self, n):
    while len(self.entropy)<n:
      if self.pers:
        result=skein512(nonce=self.iv, mac=self.key, pers=self.pers, digest_bits=(IV_SIZE+BLOCK_SIZE)*8).digest()
      else:
        result=skein512(nonce=self.iv, mac=self.key, digest_bits=(IV_SIZE+BLOCK_SIZE)*8).digest()
      self.iv, entropy=splitField(result, IV_SIZE)    
      self.entropy=self.entropy+entropy
    b, self.entropy=splitField(self.entropy, n)
    return b

  def encrypt(self, data):
    l=len(data)
    entropy=self.getBytes(l)
    cdata=bytearray()
    for x in range(l):
      cdata.append(data[x] ^ entropy[x])
    return bytes(cdata)
    
  def decrypt(self, data):
    return self.encrypt(data)