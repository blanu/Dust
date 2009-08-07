from invite.invite import loadInvitePackage
from intro.intro_packet import IntroPacket
from intro.intro_socket import intro_socket
from core.util import getAddress, getPublicIP
from crypto.curve import Key

class Introducer:
  def __init__(self, keys, myaddrKey):
    self.keys=keys
    self.myaddrKey=myaddrKey

  def acceptIntroduction(self, data, addr):
    print('Introducing', addr)

    print('myaddrkey:', self.myaddrKey)
    choices=self.keys.incomingInvites.getInvitesForAddress(self.myaddrKey)
    
    intro=IntroPacket()
    intro.decodeIntroPacket(choices, data)
    print('intro:', intro.intro.pubkey, addr)
    self.keys.addHost(addr, intro.intro.pubkey)
    
  def makeIntroduction(self, addr, sock):
    print('Introducing', addr)
    invite=self.keys.outgoingInvites.getInviteForHost(addr)
    if not invite:
      print('Can\'t find invite for', addr, ', invite failed.')
      return None

    isock=intro_socket(self.keys, socket=sock)
    isock.iconnect(invite)
    isock.isend()
    
    self.keys.addHost(invite.address, Key(invite.pubkey, False))
    
    return self.keys.getSessionKeyForHost(addr)
    