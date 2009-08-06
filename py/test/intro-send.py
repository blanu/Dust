import sys
from intro.intro_socket import *
from crypto.keys import KeyManager
from invite.invite import loadInvitePackage
from core.util import getPublicIP

buffsize=102400
host = '::'
inport=8001
dest=getPublicIP()
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