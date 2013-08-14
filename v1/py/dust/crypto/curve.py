import os
import struct
import binascii
from ctypes import *

if os.name=='nt':
  module='curve25519-donna.dll'
  funcname='curve25519_donna'
elif os.name=='posix':
  module='curve25519-donna.dylib'
  funcname='curve25519_donna'
else:
  module='curve25519.so'
  funcname='curve25519_athlon'

try:
  filename=os.path.join(os.path.dirname(__file__), module)
  print('filename: '+str(filename))
  ec=cdll.LoadLibrary(filename)
  curve25519=getattr(ec, funcname)
except Exception as e:
  print('e: '+str(e))
  module='curve25519-donna-c64.so'
  funcname='curve25519_donna'
  filename=os.path.join(os.path.dirname(__file__), module)
  ec=cdll.LoadLibrary(filename)
  curve25519=getattr(ec, funcname)

def createKeypair(entropy):
  secret=Key(createSecret(entropy), False)
  public=Key(createPublicKey(secret.buffer), True)
  return Keypair(secret, public)

def loadKeypair(sfilename, pfilename):
  secret=loadKey(sfilename)
  public=loadKey(pfilename)
  return Keypair(secret, public)

class Keypair:
  def __init__(self, secret, public):
    self.secret=secret
    self.public=public

  def createSession(self, public2):
    return Key(createShared(self.secret.buffer, public2.buffer), True)

  def createSessionBytes(self, public2):
    return createShared(self.secret.buffer, Key(public2, False).buffer)

  def save(self, sfilename, pfilename):
    self.secret.save(sfilename)
    self.public.save(pfilename)

  def __repr__(self):
    return "<Keypair(secret=%s, public=%s)>" % (self.secret, self.public)

def loadKey(filename):
  f=open(filename, 'rb')
  bytes=f.read()
  f.close()

  return Key(bytes, False)

class Key:
  def __init__(self, s, isCtype):
    if isCtype:
      self.buffer=s
      self.bytes=s.raw
    else:
      self.bytes=s
      self.buffer=create_string_buffer(s)

  def save(self, filename):
    f=open(filename, 'wb')
    f.write(self.bytes)
    f.close()

  def __repr__(self):
    return "Key<%s>" % (binascii.hexlify(self.bytes))

def createSecret(entropy):
  secret=bytearray(entropy.getBytes(32))

  secret[0] = secret[0] & 248
  secret[31] = secret[31] & 127
  secret[31] = secret[31] | 64

  return bytes(secret)

def createPublicKey(secret):
  bps=bytes([9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
  pubkey=create_string_buffer(32)

  # Compute public key from private key
  curve25519(pubkey, secret, bps)

  return pubkey

def createShared(secret, pubkey2):
  shared=create_string_buffer(32)

  # Compute shared key
  curve25519(shared, secret, pubkey2)

  return shared

if __name__=='__main__':
  s1=createSecret()
  p1=createPublicKey(s1)

  s2=createSecret()
  p2=createPublicKey(s2)

  shared=createShared(s1, p2)
  shared2=createShared(s2, p1)

  print('s1:', binascii.hexlify(s1))
  print('p1:', binascii.hexlify(p1.raw))
  print('s2:', binascii.hexlify(s2))
  print('p2:', binascii.hexlify(p2))
  print('shared1:', binascii.hexlify(shared.raw))
  print('shared2:', binascii.hexlify(shared2.raw))

  print()
  print('---')
  print()

  kp1=createKeypair()
  print('kp1:', kp1)
  kp1.save('A-priv.txt', 'A-pub.txt')

  kp2=createKeypair()
  print('kp1:', kp2)
  kp2.save('B-priv.txt', 'B-pub.txt')

  kp3=loadKeypair('A-priv.txt', 'A-pub.txt')
  print('kp3:', kp3)

  kp4=loadKeypair('B-priv.txt', 'B-pub.txt')
  print('kp4:', kp4)

  p1=loadKey('A-pub.txt')
  print('p1:', p1)

  p2=loadKey('B-pub.txt')
  print('p2:', p2)

  s1=kp1.createSession(p2)
  print('s1:', s1)

  s2=kp2.createSession(p1)
  print('s2:', s2)

  s3=kp3.createSession(p2)
  print('s3:', s3)

  s4=kp4.createSession(p1)
  print('s4:', s4)
