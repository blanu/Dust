import sys
import yaml
import random
import binascii

from crypto.keys import KeyManager
from core.util import getPublicIP, encode, decode, encodeAddress, decodeAddress
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
      map=self.invites[encode(invite.pubkey.bytes)+'/'+invite.address]
    except:
      map={}
      self.invites[encode(invite.pubkey.bytes)+'/'+encodeAddress(invite.address)]=map
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
    ser=[]
    for address in self.invites:
      map=self.invites[address]
      for id in map:
        invite=map[id]
        ser.append(invite.serialize())
    return ser
    
  def unserialize(self, ser):
    print('ser:', ser)
    for serl in ser:
      invite=Invite()
      invite.unserialize(serl)
      self.addInvite(invite)

class Invite:
  def __init__(self):
    self.pubkey=None
    self.address=None
    self.identifier=None
    self.secret=None
    
  def generate(self, pubkey, port):
    self.pubkey=pubkey
    self.address=(getPublicIP(), port)
    print('address:', self.address)
    self.identifier=self.makeIdentifier()
    self.secret = self.makeSecret()
        
  def makeIdentifier(self):
    return bytes(random.randint(0, 255) for _ in range(16))
      
  def makeSecret(self):
    return bytes(random.randint(0, 255) for _ in range(16))
      
  def serialize(self):
    return [encode(self.pubkey.bytes), encodeAddress(self.address), encode(self.identifier), encode(self.secret)]
    
  def unserialize(self, serl):
    self.pubkey=Key(decode(serl[0]), False)
    self.address=decodeAddress(serl[1])
    self.identifier=decode(serl[2])
    self.secret=decode(serl[3])
    
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