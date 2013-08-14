import sys
import time
import yaml
import email
import smtplib

from dust.core.dust_socket import *
from dust.crypto.curve import loadKeypair

f=open('emailServer.yaml', 'r')
config=yaml.load(f.read())
f.close()

buffsize=102400
host = '::1'
inport=7000
outport=7001
nodeName='A'

keypair=loadKeypair(nodeName+'-priv.txt', nodeName+'-pub.txt')

ecsock=dust_socket(keypair)
ecsock.bind(('', inport))
#ecsock.connect((host, outport))

msg, addr=ecsock.recvfrom(1024)

print('-----------------')
print(msg.decode('ascii'))

msg=msg.decode('ascii')
mail=email.message_from_string(msg)
to=mail['To']
frm=mail['From']

print('To:', to, 'From:', frm)

tod=to.split('@')[1]
frmd=frm.split('@')[1]

addressKey=addr[0]+'-'+str(addr[1])

sender=config['senders'][addressKey]
if not sender:
  print('Unknown sender', addr)
else:
  if not tod in sender['to']:
    print('Illegal to address', tod, sender['to'])
  elif not frmd in sender['from']:
    print('Illegal from address', frmd, sender['from'])
  else:
    print('Sending...')
#    smtp = smtplib.SMTP(config['smtpHost'])
#    smtp.set_debuglevel(1)
#    smtp.sendmail(frm, to, msg)
#    smtp.quit()

