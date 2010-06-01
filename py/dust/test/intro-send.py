import sys
from dust.intro.intro_socket import *
from dust.crypto.keys import KeyManager
from dust.core.util import getPublicIP

passwd=sys.argv[1]
ipv=sys.argv[2]
if ipv=='4':
  v6=False
else:
  v6=True  

host=getPublicIP(v6)
dest=getPublicIP(v6)
  
buffsize=102400
inport=8001
outport=7000
nodeName='A'

keys=KeyManager()
keys.loadKeypair('config/id.yaml')
keypair=keys.getKeypair()

keys.setInvitePassword(passwd)
keys.loadOutgoingInvites('config/outgoing_invites.ip')
invite=keys.outgoingInvites.getInviteForHost(False, (dest, outport))

isock=intro_socket(keys)
isock.bind((host, inport))
isock.iconnect(invite)
isock.isend()