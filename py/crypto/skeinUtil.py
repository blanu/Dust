import random

from skein import skein512
from core.utils import splitFields

SEED_SIZE=16

class SkeinPRNG:
  def __init__(self, seed=None):
    if seed:
      self.seed=seed
    else:
      self.seed=generateSeed()
      
  def generateSeed(self):
    return bytes(random.randint(0, 255) for _ in range(SEED_SIZE))

  def reseed(self, seed):
    self.seed=seed
    
  def getBytes(self, n):
    result=skein512(self.seed, digest_size=SEED_SIZE+n)
    self.seed, r=splitFields(result, [SEED_SIZE, n])
    return r
