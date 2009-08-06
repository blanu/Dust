import sys
from intro.intro_socket import *
from crypto.keys import KeyManager
from invite.invite import loadInvitePackage
from core.util import getPublicIP

ipv=sys.argv[1]
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

invites=loadInvitePackage('config/myinvites.ip', 'test')
invite=invites.getInviteForHost(False, (dest, outport))

isock=intro_socket(keys)
isock.bind((host, inport))
isock.iconnect(invite)
isock.isend()