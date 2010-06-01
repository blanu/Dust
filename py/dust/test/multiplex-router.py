import sys
import time
import yaml
import email
import smtplib

from multiplex_socket import *
from crypto.curve import loadKeypair

from services import services
print("services:", services)

buffsize=102400
host = '::1'
inport=7000
outport=7001
nodeName='A'

keypair=loadKeypair(nodeName+'-priv.txt', nodeName+'-pub.txt')

msock=multiplex_socket(keypair)
msock.bind(('', inport))
#ecsock.connect((host, outport))

msg, addr, service=msock.mrecvfrom(1024)
handler=services[service]
print('Routing to', handler, '...')
handler(msg, addr)