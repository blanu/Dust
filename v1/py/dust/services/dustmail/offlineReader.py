#!/usr/bin/python3.1
import os
import sys
import time
import glob
import traceback

from dust.crypto.curve import Key
from dust.crypto.keys import KeyManager
from dust.extensions.onion.onion_packet import OnionPacket
from dust.util.ymap import YamlMap
from dust.core.util import getPublicIP, encode, decode, encodeAddress, decodeAddress, randomPort
from dust.invite.invite_packet import InviteMessage
from dust.services.dustmail.dustmail_packet import DustmailInvitePacket

from threading import Event
from dust.server.router import PacketRouter
from dust.util.safethread import wait, waitForEvent
from dust.extensions.onion.onion_packet import OnionPacket
from dust.services.tracker.trackerClient import TrackerClient
from dust.services.dustmail.dustmailClient import DustmailClient
from dust.services.dustmail.notify import Notifier

class PendingMessage:
  def __init__(self, reader, name, msg):
    self.reader=reader
    self.name=name
    self.msg=msg

    destAddress=self.reader.book[self.name]['tracker']
    dest, outport, v6=decodeAddress(destAddress)
    recipient=self.reader.book[self.name]['pubkey']
    self.endkey=decode(recipient)

    self.reader.trackback.setPutPeerForEndpointCallback(recipient, self.foundPeer)
    self.tracker=TrackerClient(self.reader.router, addr=(dest, outport))
    self.tracker.getPeerForEndpoint(recipient)

  def foundPeer(self, endkey, peer):
    destkey=decode(peer[0])
    addr=peer[1]

    if self.reader.keys.isKnown(addr) or self.reader.keys.outgoingInvites.getInviteForHost(False, decodeAddress(addr)):
      self.sendMessage(decodeAddress(addr))
    else:
      self.reader.trackback.setPutInviteForPeerCallback(addr, self.foundInvite)
      self.tracker.getInviteForPeer(addr)

  def foundInvite(self, addr, invite):
    self.sendMessage(decodeAddress(addr))

  def sendMessage(self, addr):
    data=self.msg.encode('ascii')
    onion=OnionPacket()
    onion.createOnionPacket(self.reader.keys.getEndpoint(), self.endkey, data, self.reader.keys.entropy)
    dustmail=DustmailClient(self.reader.router, addr)
    dustmail.sendMessage(encode(onion.packet))

    self.reader.commandDone.set()

class DustmailReader:
  def __init__(self, router, endpoint):
    self.router=router
    self.endpoint=endpoint

    self.keys=router.keys
    self.maildir='spool/'+encode(endpoint.public.bytes)
    self.addressBook=YamlMap('config/dustmail-addressbook.yaml')
    self.done=Event()
    self.commandDone=Event()

    self.book=YamlMap('config/dustmail-addressbook.yaml')

    dustmailConfig=YamlMap('config/dustmail-config.yaml')

    try:
      destAddress=dustmailConfig['tracker']
    except:
      entry=self.addInvite()
      destAddress=entry['tracker']
      dustmailConfig['tracker']=destAddress

    dest, outport, v6=decodeAddress(destAddress)

    host=getPublicIP(v6)
    inport=dustmailConfig['port']

    invite=self.keys.generateInvite(inport, v6=v6)

  def start(self):
    print('-'*40)
    msgs=self.displayList()

    command=None
    while command!='x':
      command=input('> ').strip()
      try:
        num=int(command)
        self.displayMessage(msgs[num-1][1])
      except:
        if command=='l':
          msgs=self.displayList()
        else:
          self.parseCommand(command)

  def displayList(self):
    #  msgs=os.listdir(maildir)
    msgs=[]
    for file in glob.glob(self.maildir + '/*.*'):
      stats = os.stat(file)
      lastmod_date = time.localtime(stats[8])
      date_file_tuple = lastmod_date, file
      msgs.append(date_file_tuple)

    if len(msgs)==0:
      print("No messages.")
    else:
      msgs.sort()
      msgs.reverse()

      for x in range(len(msgs)):
        date, fname=msgs[x]
        frm=fname.split('/')[-1].split('-')[0]
        modtime=time.strftime("%m/%d/%Y %I:%M%p",date)
        frmName=self.nameForPubkey(frm)
        if not frmName:
          frmName=frm
        print(str(x+1)+': '+frmName+' '+modtime)
    return msgs

  def nameForPubkey(self, pubkey):
    for name in self.book.keys():
      key=self.book[name]['pubkey']
      if key==pubkey:
        return name
    return None

  def displayMessage(self, fname):
    f=open(fname, 'r')
    msg=f.read()
    f.close()

    data=decode(msg)

    onion=OnionPacket()
    #print(onion)
    try:
      onion.decodeOnionPacket(self.endpoint, data)
    except:
      traceback.print_exc()
    #print(onion)
    print(onion.data.decode('ascii'))

  def parseCommand(self, command):
    self.commandDone.clear()
    if command=='x':
      self.commandDone.set()
      self.done.set()
      sys.exit(0)
    elif command=='a':
      self.addInvite()
    elif command=='i':
      self.makeInvite()
    elif command=='s':
      self.sendMessage()
    elif command=='?':
      self.printHelp()
    waitForEvent(self.commandDone)

  def addInvite(self):
    pf=input("Load invite from Paste or File [P/f]? ")
    if pf=='f':
      filename=input("Load invite from filename: ").strip()
      f=open(filename, 'rb')
      data=f.read()
      f.close()
    else:
      data=decode(input("Past invite: "))

    passwd=input("Decrypt invite with password: ")
    packet=DustmailInvitePacket()
    packet.decodeDustmailInvitePacket(passwd, data)
    invite=InviteMessage()
    invite.decodeInviteMessage(packet.invite)
    self.keys.addInvite(invite)

    name=input("Name for this endpoint: ")
    try:
      entry=self.book[name]
    except:
      entry={}
    entry['pubkey']=encode(packet.pubkey)
    entry['tracker']=encodeAddress((invite.ip, invite.port))
    self.book[name]=entry

    self.commandDone.set()

    return entry

  def makeInvite(self):
    self.trackback.setPutTrackerInviteCallback(self.gotInvite)
    self.tracker.getTrackerInvite()

  def gotInvite(self, invite):
    time.sleep(1)
    print()
    ps=input("Print, Save, or Email [P/s/e]? ")
    passwd=input("Encrypt invite with password: ")
    packet=DustmailInvitePacket()
    packet.createDustmailInvitePacket(passwd, self.endpoint.public.bytes, invite, self.keys.entropy)
    if ps=='s':
      filename=input("Save invite to filename: ").strip()
      if filename!='':
        f=open(filename, 'wb')
        f.write(packet.packet)
        f.close()
    elif ps=='e':
      frm=input("Your email: ")
      to=input("Recipient email: ")
      name=input("Your name on DustMail: ")

      body="""
      You have been invited to communicate with %s via DustMail.
      Use the following invite code: %s
      """ % (name, encode(packet.packet))

      emailConfig=YamlMap('config/emailServer.yaml')
      try:
        smtpHost=emailConfig['smtpHost']
      except:
        smtpHost=input("SMTP Host: ")
        emailConfig['smtpHost']=smtpHost

      notifier=Notifier(frm)
      notifier.notify(to, 'DustMail Invite', body)
    else:
      print()
      print(encode(packet.packet))
      print()

    self.commandDone.set()

  def sendMessage(self):
    name=input("To: ")
    message=input("Message: ")

    PendingMessage(reader, name, message)

  def printHelp(self):
    print('num: read message num')
    print('x: quit')
    print('l: list messages')
    print('a: add invite')
    print('i: make invite')
    print('s: send message')

    self.commandDone.set()

if __name__=='__main__':
  if len(sys.argv)>1 and sys.argv[1]=='-d':
    passwd=sys.argv[2]
    headless=True
  else:
    passwd=input("Password: ")
    headless=False

  dustmailConfig=YamlMap('config/dustmail-config.yaml')

  try:
    inport=int(dustmailConfig['port'])
  except:
    inport=randomPort()
    dustmailConfig['port']=inport

  print('Loading keys...')
  keys=KeyManager()
  keys.setInvitePassword(passwd)
  keys.loadKnownHosts('config/knownhosts.yaml')

  try:
    keys.loadKeypair('config/id.yaml')
  except:
    print('Generating server keypair...')
    keys.createKeypair()
    keys.saveKeypair('config/id.yaml')

  keys.loadIncomingInvites('config/incoming_invites.ip')
  keys.loadOutgoingInvites('config/outgoing_invites.ip')

  try:
    endpoint=keys.loadEndpoint(os.path.expanduser('~/.dust/endpoint.yaml'))
  except:
    print('Generating endpoint keypair...')
    keys.createKeypair()
    dustdir=os.path.expanduser("~/.dust")
    if not os.path.exists(dustdir):
      os.mkdir(dustdir)
    keys.saveKeypair(dustdir+'/endpoint.yaml')

  services=YamlMap('config/activeServices.yaml')
  services['dustmail']=['dust.services.dustmail.dustmailService', 'DustmailService']

  v6=True
  router=PacketRouter(v6, inport, keys, passwd)
  router.start()

  reader=DustmailReader(router, endpoint)
  if not headless:
    reader.start()
  else:
    print('Running in daemon mode.')

  waitForEvent(reader.done)
