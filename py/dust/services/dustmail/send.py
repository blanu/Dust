import os
import sys
import time
from dust.crypto.keys import KeyManager
from dust.crypto.curve import Key
from dust.core.util import getPublicIP, encode, decode, encodeAddress, decodeAddress
from dust.core.data_packet import DataPacket
from dust.server.router import PacketRouter
from dust.util.safethread import wait
from dust.invite.invite_packet import InviteMessage

from dust.services.tracker.trackerClient import TrackerClient
from dust.services.dustmail.dustmailClient import DustmailClient

passwd=sys.argv[1]
inport=int(sys.argv[2])
destAddress=sys.argv[3]
dest, outport, v6=decodeAddress(destAddress)
recipient=sys.argv[4]
message=sys.argv[5]

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

class PendingMessage:
  def __init__(self, keys, router, tracker, trackback, keypair, endkey, msg):
    self.keys=keys
    self.router=router
    self.keypair=keypair
    self.endkey=endkey
    self.msg=msg
    trackback.setPutPeerForEndpointCallback(encode(endkey.bytes), self.foundPeer)
    tracker.getPeerForEndpoint(encode(endkey.bytes))

  def foundPeer(self, endkey, peer):
    print('foundPeer!!! '+str(endkey)+' '+str(peer))
    destkey=decode(peer[0])
    addr=peer[1]

    if keys.isKnown(addr) or self.keys.outgoingInvites.getInviteForHost(False, decodeAddress(addr)):
      self.sendMessage(destkey, decodeAddress(addr))
    else:
      trackback.setPutInviteForPeerCallback(addr, self.foundInvite)
      tracker.getInviteForPeer(addr)

  def foundInvite(self, addr, invite):
    self.sendMessage(invite.pubkey, decodeAddress(addr))

  def sendMessage(self, destkey, addr):
    data=self.msg.encode('ascii')
    sessionKey=self.keypair.createSession(Key(destkey, False))
    print('session '+str(sessionKey.bytes))
    packet=DataPacket()
    packet.createDataPacket(sessionKey.bytes, data, self.keys.entropy)
    dustmail=DustmailClient(self.router, addr)
    dustmail.sendMessage(encode(self.keypair.public.bytes), encode(destkey), encode(packet.packet))

router=PacketRouter(v6, inport, keys, passwd)
router.connect(dest, outport)

tracker=TrackerClient(router)
trackback=router.getService('trackback')

router.start()

tracker.putPeerForEndpoint(pubkeyhex, [pubkeyhex, encodeAddress((host,inport))])

pending=PendingMessage(keys, router, tracker, trackback, keypair, destpubkey, message)

wait()
