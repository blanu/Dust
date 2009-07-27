import sys
import time
import struct
from socket import *

import yaml

from crypto.curve import *
from intro.intro_packet import IntroPacket
from invite.invite import loadInvitePackage
from core.util import encodeAddress

class intro_socket:
  def __init__(self, keys, socket=None):
    self.keys=keys
    self.pubkey=keys.getKeypair().public
    if socket:
      self.sock=socket
    else:
      self.sock=socket(AF_INET6, SOCK_DGRAM)
    
  def bind(self, address):
    print('binding', address)
    self.sock.bind(address)
    
  def iconnect(self, invite):
    self.invite=invite
    
  def isend(self):
    if not self.invite:
      print('No invite')
      return      
    print('invite address:', self.invite.address)
    packet=IntroPacket()
    packet.createIntroPacket(self.invite.secret, self.invite.identifier, self.pubkey)
    self.sock.sendto(packet.packet, 0, self.invite.address)
    self.invite=None # Invites are single use only