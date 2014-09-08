from dust.core.dust_packet import DustPacket
from dust.core.util import getAddress, encode, splitField
from dust.crypto.dustUtil import pbkdf

SALT_LENGTH=32
PUBKEY_LENGTH=32

class DustmailInvitePacket(DustPacket):
  def __init__(self):
    DustPacket.__init__(self)

    self.salt=None
    self.pubkey=None
    self.invite=None

  def makeSalt(self, entropy):
    return entropy.getBytes(SALT_LENGTH)

  def createDustmailInvitePacket(self, password, pubkey, invite, entropy):
    self.pubkey=pubkey
    self.invite=invite
    self.salt=self.makeSalt(entropy)
    sk=pbkdf(password, self.salt)
    message=self.pubkey+self.invite
    self.createDustPacket(sk, message, entropy)
    self.packet=self.salt+self.packet

  def decodeDustmailInvitePacket(self, password, packet):
    self.salt, packet=splitField(packet, SALT_LENGTH)
    sk=pbkdf(password, self.salt)
    self.decodeDustPacket(sk, packet)
    self.pubkey, self.invite=splitField(self.data, PUBKEY_LENGTH)
