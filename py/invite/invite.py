import sys
import yaml
import random
import binascii

from crypto.keys import KeyManager
from core.util import getAddress, encode, decode, encodeAddress, decodeAddress
from core.ec_packet import makeIV, encrypt
from skein import skein256

def createInvitePackage(pubkey, port, number):
  ip=InvitePackage()
  ip.generate(pubkey, port, number)
  return ip

def loadInvitePackage(filename):
  ip=InvitePackage()
  ip.load(filename)
  return ip

class InvitePackage:
  def __init__(self):
    self.invites={}

  def getInviteForHost(self, address):
    return self.getInviteForAddress(encodeAddress(address))
    
  def getInviteForAddress(self, addressKey):
    invites=self.getInvitesForAddress(addressKey)
    if invites:
      invite=invites.popitem()[1]
      return invite
    else:
      return None
    
  def getInvitesForHost(self, address):
    return self.getInvitesForAddress(encodeAddress(address))
    
  def getInvitesForAddress(self, addressKey):
    print('getInvitesForAddress('+addressKey+')')
    for address in self.invites:
      print('address:', address)
      pubkey, addr=address.split('/')
      print('addr:', addr)
      if addr==addressKey:
        return self.invites[address]
    return None
    
  def merge(self, ip):
    for address in ip.invites:
      invites=ip.invites[address]
      for id in invites:
        invite=invites[id]
        print('invite:', invite)
        self.addInvite(invite)
    
  def addInvite(self, invite):
    try:
      map=self.invites[encode(invite.pubkey)+'/'+invite.address]
    except:
      map={}
      self.invites[encode(invite.pubkey)+'/'+encodeAddress(invite.address)]=map
    map[invite.identifier]=invite
    
  def removeInvite(self, address, id):
    del self.invites[address][id]
    
  def generate(self, pubkey, port, number):
    for x in range(number+1):
      i=Invite()
      i.generate(pubkey, port)
      self.addInvite(i)
      
  def load(self, filename, password=None):
    try:
      f=open(filename, 'rb')
    except:
      print('No such file', filename)
      return
      
    data=f.read()
    if password:
      data=decode(data)
      iv=data[:16]
      data=data[16:]
      k=skein256(password).digest()
      data=decrypt(k, iv, data)        
    else:
      data=data.decode('ascii')
    self.unserialize(yaml.load(data))
    f.close()
      
  def save(self, filename, password=None):
    f=open(filename, 'wb')
    print('going to write:', self.serialize())
    data=yaml.dump(self.serialize())
    if password:
      iv=makeIV()
      k=skein256(password).digest()
      data=iv+encrypt(k, iv, data)
      data=encode(data)
    else:
      data=data.encode('ascii')
    f.write(data)
    f.close()
    
  def serialize(self):
    ser={}
    for address in self.invites:
      sermap={}
      ser[address]=sermap
      map=self.invites[address]
      for id in map:
        invite=map[id]
        sermap[encode(id)]=encode(invite.secret)
    return ser
    
  def unserialize(self, ser):
    print('ser:', ser)
    for addressKey in ser:
      pubkey, address=addressKey.split('/')
      pubkey=decode(pubkey)
      address=decodeAddress(address)
      sermap=ser[addressKey]
      for id in sermap:
        secret=sermap[id]
        invite=Invite()
        invite.unserialize(pubkey, address, id, secret)
        self.addInvite(invite)

class Invite:
  def __init__(self):
    self.address=None
    self.identifier=None
    self.secret=None
    
  def generate(self, pubkey, port):
    self.pubkey=pubkey
    self.address=getAddress(port)
    print('address:', self.address)
    self.identifier=self.makeIdentifier()
    self.secret = self.makeSecret()
        
  def makeIdentifier(self):
    return bytes(random.randint(0, 255) for _ in range(16))
      
  def makeSecret(self):
    return bytes(random.randint(0, 255) for _ in range(16))
      
  def serialize(self):
    ser={encode(self.identifier): encode(self.secret)}
    return ser
    
  def unserialize(self, pubkey, address, id, secret):
    self.pubkey=pubkey
    self.address=address
    self.identifier=decode(id)
    self.secret=decode(secret)
    
if __name__=='__main__':
  keys=KeyManager()
  keys.loadKeypair('id.yaml')
  keypair=keys.getKeypair()
  print('keypair:', keypair)
  pubkey=keypair.public
  
  ip=InvitePackage()
  ip.generate(pubkey.bytes, 2001, 5)
  ip.save('test.ip')
  
  print('ip:', ip)
  
  ip2=InvitePackage()
  ip2.load('test.ip')
  print('ip2:', ip2)