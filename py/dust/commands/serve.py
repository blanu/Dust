import sys
import time

from dust.server.router import PacketRouter
from dust.crypto.keys import KeyManager
from dust.core.util import getPublicIP, encodeAddress, decodeAddress, encode
from dust.util.ymap import YamlMap
from dust.util.safethread import wait

from dust.services.tracker.trackerClient import TrackerClient

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

invite=keys.generateInvite(inport, v6=v6)
tracker.putInviteForPeer(encode(pubkey.bytes), encode(invite.message))

endpoints=YamlMap('config/endpoints.yaml')
for key in endpoints.values():
  tracker.putPeerForEndpoint(key, [encode(pubkey.bytes), encodeAddress((host,inport))])

wait()
