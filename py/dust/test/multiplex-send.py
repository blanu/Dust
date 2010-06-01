import sys
import time
from dust.extensions.multiplex.multiplex_socket import *
from dust.crypto.curve import loadKeypair

buffsize=102400
host = '::1'
outport=7000
inport=7001
nodeName='A'

keypair=loadKeypair(nodeName+'-priv.txt', nodeName+'-pub.txt')

msock=multiplex_socket(keypair)
msock.bind((host, inport))
msock.connect((host, outport))
msock.msend(b'testA', service='test1')
msock.msendto(b'testB', (host, outport), service='test2')

msock.connectToService('test3')

msock.msend(b'testC')
msock.msendto(b'testD', (host, outport))