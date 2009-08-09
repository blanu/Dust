import sys
import time
from crypto.keys import KeyManager
from multiplex.multiplex_socket import *
from core.util import getPublicIP

from email.mime.text import MIMEText

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
