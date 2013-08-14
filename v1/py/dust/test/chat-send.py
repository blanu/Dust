import sys
import time
from dust.crypto.keys import KeyManager
from dust.extensions.multiplex.multiplex_socket import *
from dust.core.util import getPublicIP, decodeAddress

passwd=sys.argv[1]
inport=int(sys.argv[2])
addr=sys.argv[3]

dest, outport, ipv=decodeAddress(addr)

host=getPublicIP(ipv)

keys=KeyManager()
keys.setInvitePassword(passwd)
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadKeypair('config/id.yaml')
keys.loadIncomingInvites('config/incoming_invites.ip')
keys.loadOutgoingInvites('config/outgoing_invites.ip')

msock=multiplex_socket(keys)
msock.bind((host, inport))
msock.connect((dest, outport))
msock.connectToService('chat')

msg=''
while msg!='.':
  print('>> ', end='')
  msg=sys.stdin.readline().strip()
  msock.msend(msg.encode('ascii'))
