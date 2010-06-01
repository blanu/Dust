import sys
import time
from dust.crypto.keys import KeyManager
from dust.core.util import getPublicIP, encodeAddress
from dust.server.router import PacketRouter

from dust.services.tracker.trackerClient import TrackerClient

def foundPeer(pubkey, peer):
  print('foundPeer '+str(pubkey)+' '+str(peer))
  pubkey=peer[0]
  addr=peer[1]

passwd=sys.argv[1]
inport=int(sys.argv[2])
dest=sys.argv[3]
outport=int(sys.argv[4])
ipv=int(sys.argv[5])
if ipv==6:
  v6=True
else:
  v6=False

host=getPublicIP(v6)
print('Host: '+str(host))

keys=KeyManager()
keys.setInvitePassword(passwd)
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadKeypair('config/id.yaml')
keys.loadIncomingInvites('config/incoming_invites.ip')
keys.loadOutgoingInvites('config/outgoing_invites.ip')

router=PacketRouter(v6, inport, keys, passwd)
router.connect(dest, outport)

tracker=TrackerClient(router)
trackback=router.getService('trackback')

router.start()

tracker.putPeerForEndpoint('43aafb64bc96460f3928f6068b2a01aa87bac16da6dc034b4525d1837e9cb85e', ['43aafb64bc96460f3928f6068b2a01aa87bac16da6dc034b4525d1837e9cb85e', encodeAddress((host, inport))])
trackback.setPutPeerForEndpointCallback('43aafb64bc96460f3928f6068b2a01aa87bac16da6dc034b4525d1837e9cb85e', foundPeer)
tracker.getPeerForEndpoint('43aafb64bc96460f3928f6068b2a01aa87bac16da6dc034b4525d1837e9cb85e')

while True:
  time.sleep(1)
