import sys
import time
import struct
from socket import *

import yaml

from dust.crypto.curve import *
from dust.intro.intro_packet import IntroPacket
from dust.core.util import encodeAddress

class intro_socket:
  def __init__(self, keys, socket=None):
    self.keys=keys
    self.pubkey=keys.getKeypair().public
    if socket:
      self.sock=socket
    else:
      self.sock=None
    self.address=None
    
  def bind(self, address):
    print('binding', address)
    self.address=address
    
  def iconnect(self, invite):
    self.invite=invite
    
  def isend(self):
    if not self.invite:
      print('No invite')
      return      
    
    if not self.sock:
      if self.invite.v6:
        self.sock=socket(AF_INET6, SOCK_DGRAM)
      else:
        self.sock=socket(AF_INET, SOCK_DGRAM)
        
    if self.address:
      try:
        self.sock.bind(self.address)      
      except:
        if self.invite.v6:
          self.sock.bind(('::',self.address[1]))
        else:
          self.sock.bind(('',self.address[1]))
      
    packet=IntroPacket()
    packet.createIntroPacket(self.invite.secret, self.invite.id, self.pubkey, self.keys.entropy)
    addr=(self.invite.ip, self.invite.port)
    self.sock.sendto(packet.packet, 0, addr)
    self.invite=None # Invites are single use only
    