import os
import sys
# python sucks
sys.path.insert(0, os.path.realpath(os.path.join(os.path.dirname(__file__), "..")))

from invite.invite import InvitePackage
from crypto.keys import KeyManager

password=sys.argv[1]

v6=True # defaults to IPv6
if len(sys.argv)>2:
  if sys.argv[2]=='6':
    v6=True
  elif sys.argv[2]=='4':
    v6=False
  else:
    print('Unknown IP version:', sys.argv[2])

keys=KeyManager()
keys.loadKeypair('config/id.yaml')
keypair=keys.getKeypair()
pubkey=keypair.public

ip=InvitePackage()
ip.generate(pubkey, v6, False, 7000, 5, keys.entropy)
ip.save('config/incoming_invites.ip', password, keys.entropy)
