import os
import sys
import time
import glob
from dust.crypto.curve import Key
from dust.crypto.keys import KeyManager
from dust.core.util import getPublicIP, encode, decode
from dust.extensions.onion.onion_packet import OnionPacket
from dust.util.ymap import YamlMap

passwd=sys.argv[1]

keys=KeyManager()
keys.setInvitePassword(passwd)
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadKeypair('config/id.yaml')
keys.loadIncomingInvites('config/incoming_invites.ip')
keys.loadOutgoingInvites('config/outgoing_invites.ip')
endpoint=keys.loadEndpoint(os.path.expanduser('~/.dust/endpoint.yaml'))

pubkey=endpoint.public
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
    frmName=nameForPubkey(addressBook, frm)
    if not frmName:
      frmName=frm
    print(str(x+1)+': '+frmName+' '+modtime)
  return msgs

def nameForPubkey(book, pubkey):
  for name in book.keys():
    key=book[name]['pubkey']
    if key==pubkey:
      return name
  return None

def displayMessage(fname):
  f=open(fname, 'r')
  msg=f.read()
  f.close()

  data=decode(msg)

  onion=OnionPacket()
  onion.decodeOnionPacket(endpoint, data)
#  print(onion)
  print(onion.data.decode('ascii'))

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
