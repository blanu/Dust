import sys

from server.router import PacketRouter
from crypto.keys import KeyManager

inport=int(sys.argv[1])
v6=sys.argv[2]
passwd=sys.argv[3]

keys=KeyManager()
keys.loadKeypair('config/id.yaml')
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadIncomingInvites('config/incoming_invites.ip')
keys.loadOutgoingInviteS('config/outgoing_invites.ip')

PacketRouter(v6, inport, keys, passwd)

