import sys
import yaml
import random
import binascii

from skein import skein256

from crypto.keys import KeyManager
from core.util import getPublicIP, encode, decode, encodeAddress, decodeAddress
from core.ec_packet import makeIV, encrypt

from invite.invite_packet import InviteMessage, InvitePacket

def createInvitePackage(pubkey, v6, tcp, port, number):
  ip=InvitePackage()
  ip.generate(pubkey, v6, tcp, port, number)
  return ip

def loadInvitePackage(filename, password):
  ip=InvitePackage()
  ip.load(filename, password)
  return ip

class InvitePackage:
  def __init__(self):
    self.invites=[]
    
  def __str__(self):
    s='['
    for invite in self.invites:
      s=s+str(invite)+', '
    if len(s)>1:
      s=s[:-2]
    s=s+']'
    return s

  def getInviteForHost(self, tcp, address):
    for invite in self.invites:
      if invite.tcp==tcp and invite.ip==address[0] and invite.port==address[1]:
        return invite
    return None
        
  def getInvitesForHost(self, tcp, address):
    results=[]
    for invite in invites:
      if invite.tcp==tcp and invite.ip==address[0] and invite.port==address[1]:
        return results.append(invite)
    return results
    
  def merge(self, ip):
    for invite in ip.invites:
      self.addInvite(invite)
    
  def addInvite(self, invite):
    if not invite in self.invites:
      self.invites.append(invite)
      
  def removeInvite(self, invite):
    self.invites.remove(invite)
    
  def generate(self, pubkey, v6, tcp, port, number):
    for x in range(number+1):
      i=InviteMessage()
      i.generate(pubkey, v6, tcp, port)
      self.addInvite(i)
      
  def load(self, filename, password):
    try:
      f=open(filename, 'r')
    except:
      print('No such file', filename)
      return

    for line in f.readlines():
      data=decode(line.strip())
      packet=InvitePacket()
      packet.decodeInvitePacket(password, data)
      self.addInvite(packet.invite)

    f.close()
      
  def save(self, filename, password):
    f=open(filename, 'w')

    for invite in self.invites:
      packet=InvitePacket()
      packet.createInvitePacket(password, invite)
      data=encode(packet.packet)
      f.write(data)
      f.write("\n")
      
    f.close()
      
if __name__=='__main__':
  keys=KeyManager()
  keys.loadKeypair('config/id.yaml')
  keypair=keys.getKeypair()
  print('keypair:', keypair)
  pubkey=keypair.public
  
  ip=InvitePackage()
  ip.generate(pubkey, True, False, 2001, 5)
  ip.save('test.ip', 'test')
  
  print('ip:', ip)
  
  ip2=InvitePackage()
  ip2.load('test.ip', 'test')
  print('ip2:', ip2)