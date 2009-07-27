import struct
import random
import binascii
from ctypes import *

ec=cdll.LoadLibrary('./crypto/curve25519.so')

def createKeypair():
  secret=Key(createSecret(), False)
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

def createSecret():
  keybytes=[]
  for x in range(32):
    r=random.getrandbits(8)
    keybytes.append(r)
  keybytes[0] = keybytes[0] & 248
  keybytes[31] = keybytes[31] & 127
  keybytes[31] = keybytes[31] | 64

  secret=struct.pack('32B', *keybytes)
  
  return secret
  
def createPublicKey(secret):
  basepoint=[9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  bps=struct.pack('32B', *basepoint)
  pubkey=create_string_buffer(32)

  # Compute public key from private key
  ec.curve25519_athlon(pubkey, secret, bps)
  
  return pubkey

def createShared(secret, pubkey2):
  shared=create_string_buffer(32)

  # Compute shared key
  ec.curve25519_athlon(shared, secret, pubkey2)
  
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
