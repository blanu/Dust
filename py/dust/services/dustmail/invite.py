import os
import sys
import time
from dust.crypto.keys import KeyManager
from dust.crypto.curve import Key
from dust.core.util import getPublicIP, encode, decode, encodeAddress, decodeAddress
from dust.server.router import PacketRouter
from dust.util.safethread import wait
from dust.invite.invite_packet import InviteMessage
from dust.extensions.onion.onion_packet import OnionPacket

from dust.services.tracker.trackerClient import TrackerClient
from dust.services.dustmail.dustmailClient import DustmailClient
from dust.invite.invite_packet import InviteMessage

passwd=sys.argv[1]
inport=int(sys.argv[2])
destAddress=sys.argv[3]
dest, outport, v6=decodeAddress(destAddress)
recipient=sys.argv[4]

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

destpubkey=Key(decode(recipient), False)

def gotInvite(invite):
  print('gotInvite('+str(invite)+')')

router=PacketRouter(v6, inport, keys, passwd)
router.connect(dest, outport)

tracker=TrackerClient(router)
trackback=router.getService('trackback')

router.start()

trackback.setPutTrackerInvite(self.gotInvite)
tracker.getTrackerInvite()

wait()
