import os
import sys
# python sucks
sys.path.insert(0, os.path.realpath(os.path.join(os.path.dirname(__file__), "..")))

from invite.invite import createInvitePackage
from crypto.keys import KeyManager

password=sys.argv[1]

if len(sys.argv)>2:
  ipv=sys.argv[2]
else:
  ipv=True # defaults to IPv6

keys=KeyManager()
keys.loadKeypair('config/id.yaml')
keypair=keys.getKeypair()
pubkey=keypair.public

ip=createInvitePackage(pubkey, ipv, False, 7000, 5)
ip.save('config/invites.ip', password)
