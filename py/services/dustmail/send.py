import os
import sys
import time
from crypto.keys import KeyManager
from crypto.curve import Key
from core.util import getPublicIP, encode, decode, encodeAddress, decodeAddress
from core.data_packet import DataPacket
from server.router import PacketRouter

from services.tracker.trackerClient import TrackerClient
from services.dustmail.dustmailClient import DustmailClient

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

router=PacketRouter(v6, inport, keys, passwd)
router.connect(dest, outport)

tracker=TrackerClient(router)
trackback=router.getService('trackback')

router.start()

class PendingMessage:
  def __init__(self, tracker, trackback, keypair, endkey, msg):
    self.keypair=keypair
    self.endkey=endkey
    self.msg=msg
    trackback.setPutPeerForEndpointCallback(encode(endkey.bytes), self.foundPeer)
    tracker.getPeerForEndpoint(encode(endkey.bytes))

  def foundPeer(self, endkey, peer):
    print('foundPeer!!! '+str(endkey)+' '+str(peer))
    destkey=decode(peer[0])
    addr=decodeAddress(peer[1])
    data=self.msg.encode('ascii')
    sessionKey=keypair.createSession(Key(destkey, False))
    print('session '+str(sessionKey.bytes))
    packet=DataPacket()
    packet.createDataPacket(sessionKey.bytes, data, keys.entropy)
    dustmail=DustmailClient(router, addr)
    dustmail.sendMessage(encode(self.keypair.public.bytes), encode(destkey), encode(packet.packet))

tracker.putPeerForEndpoint(pubkeyhex, [pubkeyhex, encodeAddress((host,inport))])

pending=PendingMessage(tracker, trackback, keypair, destpubkey, message)

while True:
  try:
    time.sleep(1)
  except:
    sys.exit(0)
