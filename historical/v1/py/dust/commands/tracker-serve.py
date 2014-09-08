import sys
import time

from dust.server.router import PacketRouter
from dust.crypto.keys import KeyManager
from dust.core.util import getPublicIP, encodeAddress, decodeAddress, encode

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
  try:
    time.sleep(1)
  except:
    sys.exit(0)
