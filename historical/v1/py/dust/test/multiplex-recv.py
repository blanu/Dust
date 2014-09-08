import sys
import time
from dust.extensions.multiplex_socket import *
from dust.crypto.curve import loadKeypair

buffsize=102400
host = '::1'
inport=7000
outport=7001
nodeName='A'

keypair=loadKeypair(nodeName+'-priv.txt', nodeName+'-pub.txt')

msock=multiplex_socket(keypair)
msock.bind(('', inport))
msock.connect((host, outport))
print('result:', msock.mrecvfrom(1024))
