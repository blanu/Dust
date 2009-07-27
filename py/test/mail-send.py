import sys
import time
from core.ec_socket import *
from crypto.curve import loadKeypair

from email.mime.text import MIMEText

buffsize=102400
host = '::1'
outport=7000
inport=7001
nodeName='A'

keypair=loadKeypair(nodeName+'-priv.txt', nodeName+'-pub.txt')

ecsock=ec_socket(keypair)
ecsock.bind((host, inport))
ecsock.connect((host, outport))

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
  
ecsock.send(msg.as_string().encode('ascii'))
