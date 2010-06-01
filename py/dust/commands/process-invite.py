import sys

from dust.crypto.keys import KeyManager

passwd=sys.argv[1]
mypasswd=sys.argv[2]

keys=KeyManager()
keys.setInvitePassword(mypasswd)
keys.loadOutgoingInvites('config/outgoing_invites.ip')

keys.loadIncomingInvites('config/incoming_invites.ip', passwd)
keys.outgoingInvites.merge(keys.incomingInvites)

keys.saveOutgoingInvites('config/outgoing_invites.ip', mypasswd)