import sys
import time

from server.router import PacketRouter
from crypto.keys import KeyManager
from core.util import getPublicIP, encodeAddress, decodeAddress, encode

from tracker.trackerClient import TrackerClient

inport=int(sys.argv[1])
v6=sys.argv[2]
passwd=sys.argv[3]
trackerAddr=decodeAddress(sys.argv[4])

host=getPublicIP(v6)

keys=KeyManager()
keys.setInvitePassword(passwd)
keys.loadKeypair('config/id.yaml')
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadIncomingInvites('config/incoming_invites.ip')
keys.loadOutgoingInvites('config/outgoing_invites.ip')

router=PacketRouter(v6, inport, keys, passwd)
router.connect(trackerAddr[0], trackerAddr[1])

tracker=TrackerClient(router)

router.start()

keypair=keys.getKeypair()
pubkey=keypair.public
pubkeyhex=encode(pubkey.bytes)

tracker.putPeerForEndpoint(pubkeyhex, [pubkeyhex, encodeAddress((host,inport))])

while True:
  time.sleep(1)
