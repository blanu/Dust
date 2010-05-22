import os
import sys
import time
from crypto.curve import Key
from crypto.keys import KeyManager
from core.util import getPublicIP, encode, decode
from core.data_packet import DataPacket
from util.ymap import YamlMap

passwd=sys.argv[1]

keys=KeyManager()
keys.setInvitePassword(passwd)
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadKeypair('config/id.yaml')
keys.loadIncomingInvites('config/incoming_invites.ip')
keys.loadOutgoingInvites('config/outgoing_invites.ip')

keypair=keys.getKeypair()
pubkey=keypair.public
pubkeyhex=encode(pubkey.bytes)
print('pubkey: '+pubkeyhex)
maildir='spool/'+pubkeyhex

addressBook=YamlMap('config/dustmail-addressbook.yaml')

def displayList():
  msgs=os.listdir(maildir)
  for x in range(len(msgs)):
    fname=msgs[x]
    frm=fname.split('-')[0]
    modtime=time.strftime("%m/%d/%Y %I:%M%p",time.localtime(os.path.getmtime(maildir+'/'+fname)))
    try:
      frm=addressBook[frm]
    except:
      pass
    print(str(x+1)+': '+frm+' '+modtime)
  return msgs

def displayMessage(fname):
  f=open(maildir+'/'+fname, 'r')
  msg=f.read()
  f.close()

  destpubkey=Key(decode(fname.split('-')[0]), False)
  sessionKey=keypair.createSession(destpubkey)

  data=decode(msg)
  packet=DataPacket()
  packet.decodeDataPacket(sessionKey.bytes, data)
  print(packet.data.decode('ascii'))

def parseCommand(command):
  if command=='x':
    sys.exit(0)

msgs=displayList()

command=None
while input!='x':
  command=input('> ').strip()
  try:
    num=int(command)
  except:
    if command=='l':
      msgs=displayList()
    else:
      parseCommand(command)
  displayMessage(msgs[num-1])
