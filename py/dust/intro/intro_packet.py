from dust.core.dust_packet import DustPacket
from dust.invite.invite import InvitePackage
from dust.core.util import getAddress, encode
from dust.crypto.curve import Key, Keypair

ID_LENGTH=16
PUBKEY_LENGTH=32
PRIVKEY_LENGTH=32

# 1 byte
def makeByteLength(data):
  b=bytearray(1)
  b[0]=len(data)
  print('mbl#@#@#:', b)
  return b
  
class IntroMessage:
  def __init__(self):
    self.keypair=None
    
  def createIntroMessage(self, pubkey):
    self.pubkey=pubkey
    publicKey=self.pubkey.bytes
    self.message=publicKey
    
  def decodeIntroMessage(self, message):
    self.message=message
    publicKey=self.message
    self.pubkey=Key(publicKey, False)

class IntroPacket(DustPacket):
  def __init__(self):
    DustPacket.__init__(self)

    self.identifier=None
    self.intro=None
    
  def createIntroPacket(self, sk, id, pubkey, entropy):
    self.identifier=id
    self.intro=IntroMessage()
    self.intro.createIntroMessage(pubkey)
    
    self.createDustPacket(sk, self.intro.message, entropy)
    self.packet=self.identifier+self.packet
  
  def decodeIntroPacket(self, invites, packet):
    self.identifier=packet[:ID_LENGTH]
    packet=packet[ID_LENGTH:]
    invite=invites.getInviteWithId(self.identifier)
    if not invite:
      print('Unknown invite id', encode(self.identifier))
      print(invites)
      return
    sk=invite.secret
    
    self.decodeDustPacket(sk, packet)
    self.intro=IntroMessage()
    self.intro.decodeIntroMessage(self.data)
  