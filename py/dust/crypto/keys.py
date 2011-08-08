import os
import sys
import time
import struct
from socket import *

import yaml

import dust.crypto.curve
from dust.crypto.curve import *
from dust.core.data_packet import DataPacket
from dust.core.util import encode, decode, encodeAddress
from dust.invite.invite import loadInvitePackage
from dust.crypto.dustUtil import DustPRNG, hash

class KeyManager:
  def __init__(self):
    self.knownHosts=None
    self.endpoint=None
    self.keypair=None
    self.incomingInvites=None
    self.outgoingInvites=None
    self.invitePassword=None
    self.knownHostsFile=None
    self.outgoingFilename=None
    self.entropy=DustPRNG()

  def loadKeypair(self, filename):
    f=open(filename, 'r')
    pair=yaml.load(f.read())
    f.close()
    pubkey=decode(pair[0])
    privkey=decode(pair[1])
    self.keypair=Keypair(Key(privkey, False), Key(pubkey, False))

  def loadEndpoint(self, filename):
    f=open(filename, 'r')
    pair=yaml.load(f.read())
    f.close()
    pubkey=decode(pair[0])
    privkey=decode(pair[1])
    self.endpoint=Keypair(Key(privkey, False), Key(pubkey, False))
    return self.endpoint

  def getEndpoint(self):
    return self.endpoint

  def createKeypair(self):
    self.keypair=createKeypair(self.entropy)
    return self.keypair

  def createEphemeralKeypair(self):
    return createKeypair(self.entropy)

  def saveKeypair(self, filename):
    pubkey=encode(self.keypair.public.bytes)
    privkey=encode(self.keypair.secret.bytes)
    pair=[pubkey, privkey]

    f=open(filename, 'w')
    f.write(yaml.dump(pair))
    f.close()

  def getKeypair(self):
    return self.keypair

  def loadKnownHosts(self, filename):
    self.knownHostsFile=filename
    self.knownHosts={}
    if os.path.exists(self.knownHostsFile):
      f=open(filename, 'r')
      hosts=yaml.load(f.read())
      f.close()

      for address, pubkey in hosts.items():
        self.knownHosts[address]=Key(decode(pubkey), False)

  def saveKnownHosts(self, filename=None):
    if not filename and self.knownHostsFile:
      filename=self.knownHostsFile
    if filename:
      hosts={}

      for address, pubkey in self.knownHosts.items():
        hosts[address]=encode(pubkey.bytes)

      f=open(filename, 'w')
      f.write(yaml.dump(hosts))
      f.close()

  def isKnown(self, address):
    return address in self.knownHosts

  def getKeyForHost(self, address):
    return self.knownHosts[address]

  def getHostForKey(self, key):
    for address, pubkey in self.knownHosts.items():
      print(key)
      print(type(key))
      print('==')
      print(pubkey.bytes)
      print(type(pubkey.bytes))
      if key==pubkey.bytes:
        return address
    return None

  def addHost(self, address, pubkey):
    print('addHost:', address, pubkey)
    addressKey=encodeAddress(address)
    self.knownHosts[addressKey]=pubkey
    print('knownHosts:', self.knownHosts)
    self.saveKnownHosts()

  def getSessionKeyForHost(self, address):
    addressKey=encodeAddress(address)
    return self.getSessionKeyForAddress(addressKey)

  def getSessionKeyForAddress(self, addressKey):
    try:
      pubkey=self.knownHosts[addressKey]
    except:
      print('Unknown hosts', addressKey)
      return None
    sessionKey=hash(self.keypair.createSession(pubkey).bytes)
    return sessionKey

  def setInvitePassword(self, passwd):
    self.invitePassword=passwd

  def loadIncomingInvites(self, filename, passwd=None):
    self.incomingFilename=filename
    if not passwd:
      passwd=self.invitePassword
    if passwd:
      self.incomingInvites=loadInvitePackage(filename, self.invitePassword)
    else:
      print('No invite password')

  def saveIncomingInvites(self, filename, passwd=None):
    self.incomingFilename=filename
    if not passwd:
      passwd=self.invitePassword
    if passwd and self.incomingInvites:
      self.incomingInvites.save(filename, self.invitePassword, self.entropy)
    else:
      print('No invite password or no invites')

  def generateInvite(self, port, v6=True, tcp=False):
    invites=self.incomingInvites.generate(self.keypair.public, v6, tcp, port, 1, self.entropy)
    if self.incomingFilename:
      self.saveIncomingInvites(self.incomingFilename, self.invitePassword)
    return invites[0]

  def loadOutgoingInvites(self, filename, passwd=None):
    self.outgoingFilename=filename
    if not passwd:
      passwd=self.invitePassword
    if passwd:
      self.outgoingInvites=loadInvitePackage(filename, self.invitePassword)
    else:
      print('No invite password')

  def saveOutgoingInvites(self, filename, passwd=None):
    self.outgoingFilename=filename
    if not passwd:
      passwd=self.invitePassword
    if passwd and self.outgoingInvites:
      self.outgoingInvites.save(filename, self.invitePassword, self.entropy)
    else:
      print('No invite password or no invites')

  def addInvite(self, invite):
    self.outgoingInvites.addInvite(invite)
    if self.outgoingFilename:
      self.saveOutgoingInvites(self.outgoingFilename, self.invitePassword)
