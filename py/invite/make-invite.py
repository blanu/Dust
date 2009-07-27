import invite
from crypto.keys import KeyManager

keys=KeyManager()
keys.loadKeypair('config/id.yaml')
keypair=keys.getKeypair()
pubkey=keypair.public.bytes

ip=invite.createInvitePackage(pubkey, 7000, 5)
ip.save('config/invites.ip')
