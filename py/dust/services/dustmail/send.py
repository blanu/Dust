#!/usr/bin/python3.1
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
from dust.util.ymap import YamlMap

from dust.services.tracker.trackerClient import TrackerClient
from dust.services.dustmail.dustmailClient import DustmailClient

passwd=sys.argv[1]
inport=int(sys.argv[2])
name=sys.argv[3]
message=sys.argv[4]

book=YamlMap('config/dustmail-addressbook.yaml')
destAddress=book[name]['tracker']
dest, outport, v6=decodeAddress(destAddress)
recipient=book[name]['pubkey']

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

endkey=decode(recipient)

class PendingMessage:
  def __init__(self, keys, router, tracker, trackback, keypair, endkey, msg):
    self.keys=keys
    self.router=router
    self.keypair=keypair
    self.endkey=endkey
    self.msg=msg
    trackback.setPutPeerForEndpointCallback(encode(endkey), self.foundPeer)
    tracker.getPeerForEndpoint(encode(endkey))

  def foundPeer(self, endkey, peer):
    print('foundPeer!!! '+str(endkey)+' '+str(peer))
    destkey=decode(peer[0])
    addr=peer[1]

    if keys.isKnown(addr) or self.keys.outgoingInvites.getInviteForHost(False, decodeAddress(addr)):
      self.sendMessage(decodeAddress(addr))
    else:
      trackback.setPutInviteForPeerCallback(addr, self.foundInvite)
      tracker.getInviteForPeer(addr)

  def foundInvite(self, addr, invite):
    self.sendMessage(decodeAddress(addr))

  def sendMessage(self, addr):
    print('sending message to '+str(addr))
    data=self.msg.encode('ascii')
    onion=OnionPacket()
    onion.createOnionPacket(self.keypair, self.endkey, data, self.keys.entropy)
    dustmail=DustmailClient(self.router, addr)
    dustmail.sendMessage(encode(onion.packet))

router=PacketRouter(v6, inport, keys, passwd)
router.connect(dest, outport)

tracker=TrackerClient(router)
trackback=router.getService('trackback')

router.start()

tracker.putPeerForEndpoint(pubkeyhex, [pubkeyhex, encodeAddress((host,inport))])

pending=PendingMessage(keys, router, tracker, trackback, endpoint, endkey, message)

wait()
