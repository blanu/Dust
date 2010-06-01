import sys
import time
from dust.extensions.multiplex_socket import *
from dust.crypto.curve import loadKeypair

from email.mime.text import MIMEText

buffsize=102400
host = '::1'
outport=7000
inport=7001
nodeName='A'

keypair=loadKeypair(nodeName+'-priv.txt', nodeName+'-pub.txt')

msock=multiplex_socket(keypair)
msock.bind((host, inport))
msock.connect((host, outport))
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
