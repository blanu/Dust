import sys
import time
from crypto.keys import KeyManager
from multiplex.multiplex_socket import *
from core.util import getPublicIP

passwd=sys.argv[1]
ipv=sys.argv[2]
if ipv=='4':
  v6=False
else:
  v6=True
inport=int(sys.argv[3])
outport=int(sys.argv[4])

host=getPublicIP(v6)
dest=host

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
