import os
import time
import traceback

from dust.util.ymap import YamlMap
from dust.extensions.onion.onion_packet import OnionPacket
from dust.services.dustmail.dustmailbackClient import DustmailbackClient
from dust.core.util import encode, decode
from dust.services.dustmail.notify import Notifier

class DustmailHandler:
  def __init__(self, router):
    print('new DustmailHandler')
    self.router=router
    self.dustmailback=DustmailbackClient(router)
    self.maildir='spool'

  def sendMessage(self, message):
    print('sendMessage '+str(message))

    onion=OnionPacket()
    onion.decodeOnionPacket(self.router.keys.getKeypair(), decode(message))
    frm=encode(onion.sender)
    to=encode(onion.receiver)

    print('frm: '+str(frm))
    print('to: '+str(to))

    if not os.path.exists(self.maildir+'/'+to):
      os.mkdir(self.maildir+'/'+to)

    filename=None
    while not filename or os.path.exists(filename):
      filename=self.makeName(frm, to)
    f=open(filename, 'w')
    f.write(message)
    f.close()

    notifyPrefs=YamlMap('config/dustmail-notify.yaml')
    try:
      mailAddr=notifyPrefs[to]

      addressBook=YamlMap('config/dustmail-addressbook.yaml')
      frmName=self.nameForPubkey(addressBook, frm)
      if not frmName:
        frmName=frm

      notifier=Notifier('dustmail@blanu.net')
      notifier.notify(mailAddr, 'New DustMail Message', "You have a DustMail message from "+frmName+".")
    except KeyError:
      print('No notification set')

  def makeName(self, frm, to):
    timestamp=str(time.time())
    return self.maildir+'/'+to+'/'+frm+'-'+timestamp

  def nameForPubkey(self, book, pubkey):
    for name in book.keys():
      key=book[name]['pubkey']
      if key==pubkey:
        return name
    return None
