import sys

from dust.crypto.keys import KeyManager

mypasswd=sys.argv[1]

keys=KeyManager()
keys.setInvitePassword(mypasswd)

print('Outgoing:')
keys.loadOutgoingInvites('config/outgoing_invites.ip')
print(keys.outgoingInvites)

print("\n------------------\n")

print('Incoming:')
keys.loadIncomingInvites('config/incoming_invites.ip')
print(keys.incomingInvites)
