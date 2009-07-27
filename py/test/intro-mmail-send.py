import sys
import time
from crypto.keys import KeyManager
from multiplex.multiplex_socket import *
from core.util import getPublicIP

from email.mime.text import MIMEText

buffsize=102400
host = '::'
inport=8001
dest=getPublicIP()
outport=7000

keys=KeyManager()
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadKeypair('config/id.yaml')

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
