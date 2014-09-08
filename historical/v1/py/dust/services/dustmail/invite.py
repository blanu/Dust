#!/usr/bin/python3.1
import os
import sys
import time
from threading import Event
from dust.crypto.keys import KeyManager
from dust.crypto.curve import Key
from dust.core.util import getPublicIP, encode, decode, encodeAddress, decodeAddress
from dust.server.router import PacketRouter
from dust.util.safethread import waitForEvent
from dust.invite.invite_packet import InviteMessage
from dust.extensions.onion.onion_packet import OnionPacket

from dust.services.tracker.trackerClient import TrackerClient
from dust.services.dustmail.dustmailClient import DustmailClient
from dust.invite.invite_packet import InviteMessage
from dust.services.dustmail.dustmail_packet import DustmailInvitePacket

passwd=sys.argv[1]
inport=int(sys.argv[2])
destAddress=sys.argv[3]
dest, outport, v6=decodeAddress(destAddress)
ipasswd=sys.argv[4]

host=getPublicIP(v6)
print('Host: '+str(host))

keys=KeyManager()
keys.setInvitePassword(passwd)
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadKeypair('config/id.yaml')
keys.loadIncomingInvites('config/incoming_invites.ip')
keys.loadOutgoingInvites('config/outgoing_invites.ip')
endpoint=keys.loadEndpoint(os.path.expanduser('~/.dust/endpoint.yaml'))

keypair=keys.getKeypair()
pubkey=keypair.public
pubkeyhex=encode(pubkey.bytes)

done=Event()

def gotInvite(invite):
  ps=input("Print or Save [P/s]?")
  passwd=input("Encrypt invite with password: ")
  packet=DustmailInvitePacket()
  packet.createDustmailInvitePacket(passwd, endpoint.public.bytes, invite, keys.entropy)
  if ps=='s':
    filename=input("Save invite to filename: ").strip()
    if filename!='':
      f=open(filename, 'wb')
      f.write(packet.packet)
      f.close()
  else:
    print()
    print(encode(packet.packet))
    print()

  done.set()

router=PacketRouter(v6, inport, keys, passwd)
router.connect(dest, outport)

tracker=TrackerClient(router)
trackback=router.getService('trackback')

router.start()

trackback.setPutTrackerInviteCallback(gotInvite)
tracker.getTrackerInvite()

waitForEvent(done)
