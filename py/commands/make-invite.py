import sys

from invite.invite import createInvitePackage
from crypto.keys import KeyManager

password=sys.argv[1]

keys=KeyManager()
keys.loadKeypair('config/id.yaml')
keypair=keys.getKeypair()
pubkey=keypair.public

ip=createInvitePackage(pubkey, True, False, 7000, 5)
ip.save('config/invites.ip', password)
