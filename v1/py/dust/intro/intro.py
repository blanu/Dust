from dust.invite.invite import loadInvitePackage
from dust.intro.intro_packet import IntroPacket
from dust.intro.intro_socket import intro_socket
from dust.core.util import getAddress, getPublicIP
from dust.crypto.curve import Key

class Introducer:
  def __init__(self, keys, myaddr):
    self.keys=keys
    self.myaddr=myaddr

  def acceptIntroduction(self, data, addr):
    print('Introducing', addr)

    intro=IntroPacket()
    intro.decodeIntroPacket(self.keys.incomingInvites, data)
    if intro.intro:
      self.keys.addHost(addr, intro.intro.pubkey)
      return intro
    else:
      print('Could not read intro packet')
      return None      
    
  def makeIntroduction(self, addr, sock):
    print('Introducing', addr)
    invite=self.keys.outgoingInvites.getInviteForHost(False, addr)
    if not invite:
      print('Can\'t find invite for', addr, ', invite failed.')
      return None

    print('invite:', invite)
      
    isock=intro_socket(self.keys, socket=sock)
    isock.iconnect(invite)
    isock.isend()
    
    self.keys.addHost((invite.ip, invite.port), invite.pubkey)
    
    return self.keys.getSessionKeyForHost(addr)
    