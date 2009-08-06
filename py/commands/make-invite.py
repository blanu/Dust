import os
import sys
# python sucks
sys.path.insert(0, os.path.realpath(os.path.join(os.path.dirname(__file__), "..")))

from invite.invite import createInvitePackage
from crypto.keys import KeyManager

password=sys.argv[1]

keys=KeyManager()
keys.loadKeypair('config/id.yaml')
keypair=keys.getKeypair()
pubkey=keypair.public

ip=createInvitePackage(pubkey, True, False, 7000, 5)
ip.save('config/invites.ip', password)
