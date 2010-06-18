#!/usr/bin/python3.1
import os
import sys
from dust.crypto.keys import KeyManager
from dust.crypto.curve import Key
from dust.core.util import getPublicIP, encode, decode, encodeAddress, decodeAddress
from dust.util.ymap import YamlMap

from dust.invite.invite_packet import InviteMessage
from dust.services.dustmail.dustmail_packet import DustmailInvitePacket

passwd=sys.argv[1]

keys=KeyManager()
keys.setInvitePassword(passwd)
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadKeypair('config/id.yaml')
keys.loadIncomingInvites('config/incoming_invites.ip')
keys.loadOutgoingInvites('config/outgoing_invites.ip')
endpoint=keys.loadEndpoint(os.path.expanduser('~/.dust/endpoint.yaml'))

pf=input("Load invite from Paste or File [P/f]? ")
if pf=='f':
  filename=input("Load invite from filename: ").strip()
  f=open(filename, 'rb')
  data=f.read()
  f.close()
else:
  data=decode(input("Past invite: "))

passwd=input("Decrypt invite with password: ")
packet=DustmailInvitePacket()
packet.decodeDustmailInvitePacket(passwd, data)
print("pubkey: "+encode(packet.pubkey))
print("invite: "+encode(packet.invite))
invite=InviteMessage()
invite.decodeInviteMessage(packet.invite)
keys.addInvite(invite)

name=input("Name for this endpoint: ")
book=YamlMap('config/dustmail-addressbook.yaml')
try:
  entry=book[name]
except:
  entry={}
entry['pubkey']=encode(packet.pubkey)
entry['tracker']=encodeAddress((invite.ip, invite.port))
book[name]=entry
