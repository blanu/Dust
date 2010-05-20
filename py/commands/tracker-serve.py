import sys
import time

from server.router import PacketRouter
from crypto.keys import KeyManager
from core.util import getPublicIP, encodeAddress, decodeAddress, encode

inport=int(sys.argv[1])
v6=sys.argv[2]=='6'
passwd=sys.argv[3]

host=getPublicIP(v6)

keys=KeyManager()
keys.setInvitePassword(passwd)
keys.loadKeypair('config/id.yaml')
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadIncomingInvites('config/incoming_invites.ip')
keys.loadOutgoingInvites('config/outgoing_invites.ip')

router=PacketRouter(v6, inport, keys, passwd)
router.start()

while True:
  time.sleep(1)
