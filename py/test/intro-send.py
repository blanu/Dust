import sys
import time
from intro.intro_socket import *
from crypto.keys import KeyManager
from invite.invite import loadInvitePackage
from core.util import encodeAddress

buffsize=102400
host = '::'
inport=8001
dest='2001:0:53aa:64c:18df:53c2:9c35:62b6'
outport=7000
nodeName='A'

keys=KeyManager()
keys.loadKeypair('id.yaml')
keypair=keys.getKeypair()

invites=loadInvitePackage('invites.ip')
invite=invites.getInviteForHost(encodeAddress((dest, outport)))

isock=intro_socket(keys)
isock.bind((host, inport))
isock.iconnect(invite)
isock.isend()