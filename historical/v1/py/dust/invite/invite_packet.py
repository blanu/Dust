import time
import struct
import binascii

from socket import AF_INET, AF_INET6
try:
    from socket import inet_pton, inet_ntop
except ImportError:
    from dust.invite.win32_inet_pton import inet_pton, inet_ntop

from dust.crypto.dustUtil import pbkdf

from dust.core.dust_packet import DustPacket
from dust.core.util import getPublicIP, splitFields, splitField, encodeFlags, decodeFlags, fill, encode
from dust.crypto.curve import Key

PUBKEY_LENGTH=32
FLAGS_LENGTH=1
IP_LENGTH=16
PORT_LENGTH=2
ID_LENGTH=16
SECRET_LENGTH=32
SALT_LENGTH=32

IPV4_LENGTH=4

class InviteMessage:
  def __init__(self):
    self.pubkey=None
    self.v6=None
    self.tcp=None
    self.ip=None
    self.port=None
    self.address=None
    self.id=None
    self.secret=None

  def __str__(self):
    s="[\n"
    s=s+'  pubkey: '+encode(self.pubkey.bytes)+"\n"
    s=s+'  v6:     '+str(self.v6)+"\n"
    s=s+'  tcp:    '+str(self.tcp)+"\n"
    s=s+'  ip:     '+self.ip+"\n"
    s=s+'  port:   '+str(self.port)+"\n"
    s=s+'  id:     '+encode(self.id)+"\n"
    s=s+'  secret: '+encode(self.secret)+"\n"
    s=s+"]\n"
    return s

  def generate(self, pubkey, v6, tcp, port, entropy):
    self.pubkey=pubkey
    self.v6=v6
    self.tcp=tcp
    self.ip=getPublicIP(v6)
    self.port=port
    self.id=self.makeIdentifier(entropy)
    self.secret = self.makeSecret(entropy)

    pubkey=self.pubkey.bytes
    flags=encodeFlags((self.v6, self.tcp, False, False, False, False, False, False))
    if self.v6:
      ip=inet_pton(AF_INET6, self.ip)
    else:
      #print('v4:', self.ip)
      ip=inet_pton(AF_INET, self.ip)
      #print('ip:', ip)
      ip=fill(ip, IP_LENGTH)
    port=struct.pack('H', self.port)
    id=self.id
    secret=self.secret

    self.message=pubkey+flags+ip+port+id+secret

  def makeIdentifier(self, entropy):
    return entropy.getBytes(ID_LENGTH)

  def makeSecret(self, entropy):
    return entropy.getBytes(SECRET_LENGTH)

  def createInviteMessage(self, pubkey, v6, tcp, ip, port, id, secret):
    self.pubkey=pubkey
    self.v6=v6
    self.tcp=tcp
    self.ip=ip
    self.port=port
    self.id=id
    self.secret=secret

    pubkey=self.pubkey.bytes
    flags=encodeFlags((self.v6, self.tcp, False, False, False, False, False, False))
    if self.v6:
      ip=inet_pton(AF_INET6, self.ip)
    else:
      ip=inet_pton(AF_INET, self.ip)
      ip=fill(ip, IP_LENGTH)
    port=struct.pack('H', self.port)
    id=self.id
    secret=self.secret

    self.message=pubkey+flags+ip+port+id+secret

  def decodeInviteMessage(self, message):
    self.message=message
    #print('decodeInviteMessage('+encode(message)+')'+str(len(message)))

    pubkey, flags, ip, port, id, secret=splitFields(self.message, [PUBKEY_LENGTH, FLAGS_LENGTH, IP_LENGTH, PORT_LENGTH, ID_LENGTH, SECRET_LENGTH])
    self.pubkey=Key(pubkey, False)
    flags=decodeFlags(flags)
    self.v6=flags[0]
    self.tcp=flags[1]
    if self.v6:
      self.ip=inet_ntop(AF_INET6, ip)
    else:
      ip=ip[:IPV4_LENGTH]
      self.ip=inet_ntop(AF_INET, ip)
    self.port=struct.unpack("H", port)[0]
    self.id=id
    self.secret=secret

class InvitePacket(DustPacket):
  def __init__(self):
    DustPacket.__init__(self)

    self.salt=None
    self.invite=None

  def makeSalt(self, entropy):
    return entropy.getBytes(SALT_LENGTH)

  def createInvitePacket(self, password, invite, entropy):
    self.invite=invite

    self.salt=self.makeSalt(entropy)
    sk=pbkdf(password, self.salt)
    self.createDustPacket(sk, self.invite.message, entropy)
    self.packet=self.salt+self.packet

  def decodeInvitePacket(self, password, packet):
    #print('decodeInvitePacket('+password+', '+encode(packet)+')')
    self.salt, packet=splitField(packet, SALT_LENGTH)
    #print('salt('+encode(self.salt)+', '+encode(packet)+')')
    sk=pbkdf(password, self.salt)
    #print('sk: '+encode(sk))

    self.decodeDustPacket(sk, packet)
    self.invite=InviteMessage()
    self.invite.decodeInviteMessage(self.data)
    return self.invite
