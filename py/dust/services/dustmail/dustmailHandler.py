import os
import time

from dust.util.ymap import YamlMap
from dust.extensions.onion.onion_packet import OnionPacket
from dust.services.dustmail.dustmailbackClient import DustmailbackClient

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

    if not os.path.exists(self.maildir+'/'+to):
      os.mkdir(self.maildir+'/'+to)

    filename=None
    while not filename or os.path.exists(filename):
      filename=self.makeName(frm, to)
    f=open(filename, 'w')
    f.write(message)
    f.close()

  def makeName(self, frm, to):
    timestamp=str(time.time())
    return self.maildir+'/'+to+'/'+frm+'-'+timestamp
