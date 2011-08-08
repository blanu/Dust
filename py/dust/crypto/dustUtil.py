import sys
import random
import struct

import dust.crypto.skeinUtil as skeinUtil
from dust.core.util import encode

v3=(sys.version[0]=='3')

SEED_SIZE=16
IV_SIZE=16
BLOCK_SIZE=32

PBKDF_ITERATIONS=13000

if v3:
  MAC_PERS=bytes('1978-10-26 dust@blanu.net Dust/MAC', 'ascii')
  PRNG_PERS=bytes('1978-10-26 dust@blanu.net Dust/PRNG', 'ascii')
  HASH_PERS=bytes('1978-10-26 dust@blanu.net Dust/hash', 'ascii')
  PBKDF_PERS=bytes('1978-10-26 dust@blanu.net Dust/PBKDF', 'ascii')
  CIPHER_PERS=bytes('1978-10-26 dust@blanu.net Dust/cipher', 'ascii')
else:
  MAC_PERS='1978-10-26 dust@blanu.net Dust/MAC'
  PRNG_PERS='1978-10-26 dust@blanu.net Dust/PRNG'
  HASH_PERS='1978-10-26 dust@blanu.net Dust/hash'
  PBKDF_PERS='1978-10-26 dust@blanu.net Dust/PBKDF'
  CIPHER_PERS='1978-10-26 dust@blanu.net Dust/cipher'

def hash(data):
  return skeinUtil.hash(data, digest_bits=256, pers=HASH_PERS)

def mac(key, data):
  return skeinUtil.hash(data, digest_bits=256, mac=key, pers=MAC_PERS)

def pbkdf(pb, salt):
  return skeinUtil.pbkdf(pb, salt, PBKDF_ITERATIONS, pers=PBKDF_PERS)

class DustPRNG(skeinUtil.SkeinPRNG):
  def __init__(self):
    skeinUtil.SkeinPRNG.__init__(self, pers=PRNG_PERS)

def encrypt(k, iv, data):
  cipher=DustCipher(k, iv)
  return cipher.encrypt(data)

def decrypt(k, iv, data):
  cipher=DustCipher(k, iv)
  return cipher.decrypt(data)

class DustCipher(skeinUtil.SkeinCipherOFB):
  def __init__(self, key, iv):
    if type(key)!=str:
      key=key.bytes
    skeinUtil.SkeinCipherOFB.__init__(self, key, iv, pers=CIPHER_PERS)

  def __str__(self):
    return 'DustCipher('+encode(self.key)+','+encode(self.iv)+','+str(self.count)+')'
