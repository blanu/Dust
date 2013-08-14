import sys
from dust.extensions.multiplex.lite_multiplex_socket import *
from dust.core.util import getPublicIP
from dust.crypto.keys import KeyManager

ipv=sys.argv[1]
if ipv=='4':
  v6=False
else:
  v6=True
inport=int(sys.argv[2])
outport=int(sys.argv[3])

host=getPublicIP(v6)
dest=host

keys=KeyManager()

msock=lite_multiplex_socket(keys)
msock.bind((host, inport))
msock.connect((dest, outport))
msock.connectToService('chat')

msg=''
while msg!='.':
  print('>> ', end='')
  msg=sys.stdin.readline().strip()
  msock.msend(msg.encode('ascii'))
