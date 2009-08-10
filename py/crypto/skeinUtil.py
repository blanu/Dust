import random
import struct

from skein import skein512
from core.util import splitFields

SEED_SIZE=16

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
      
def pbkdf(pb, salt, i, digest_bits=256):
  data=(pb.encode('ascii')+salt)*i
  return skein512(data, digest_bits=digest_bits).digest()