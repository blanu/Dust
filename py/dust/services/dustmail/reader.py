import os
import sys
import time
import glob
from dust.crypto.curve import Key
from dust.crypto.keys import KeyManager
from dust.core.util import getPublicIP, encode, decode
from dust.core.data_packet import DataPacket
from dust.util.ymap import YamlMap

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
#  msgs=os.listdir(maildir)
  msgs=[]
  for file in glob.glob(maildir + '/*.*'):
    stats = os.stat(file)
    lastmod_date = time.localtime(stats[8])
    date_file_tuple = lastmod_date, file
    msgs.append(date_file_tuple)

  msgs.sort()
  msgs.reverse()

  for x in range(len(msgs)):
    date, fname=msgs[x]
    frm=fname.split('/')[-1].split('-')[0]
    modtime=time.strftime("%m/%d/%Y %I:%M%p",date)
    try:
      frm=addressBook[frm]
    except:
      pass
    print(str(x+1)+': '+frm+' '+modtime)
  return msgs

def displayMessage(fname):
  f=open(fname, 'r')
  msg=f.read()
  f.close()

  destpubkey=Key(decode(fname.split('/')[-1].split('-')[0]), False)
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
  displayMessage(msgs[num-1][1])
