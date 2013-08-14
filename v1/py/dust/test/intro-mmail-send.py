import sys
import time
from dust.crypto.keys import KeyManager
from dust.extensions.multiplex.multiplex_socket import *
from dust.core.util import getPublicIP

from email.mime.text import MIMEText

inport=int(sys.argv[1])
outport=int(sys.argv[2])
ipv=sys.argv[3]
if ipv=='4':
  v6=False
else:
  v6=True
passwd=sys.argv[4]

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
msock.connectToService('mail')

frm=input('From: ')
to=input('To: ')
subject=input('Subject: ')

line=None
body=''
while True:
  line=sys.stdin.readline()
  if line.strip()=='.':
    break
  body=body+line

msg=MIMEText(body)
msg['From']=frm
msg['To']=to
msg['Subject']=subject
  
msock.msend(msg.as_string().encode('ascii'))
