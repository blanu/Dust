import os
import time

from dust.util.ymap import YamlMap
from dust.services.dustmail.dustmailbackClient import DustmailbackClient

class DustmailHandler:
  def __init__(self, router):
    print('new DustmailHandler')
    self.dustmailback=DustmailbackClient(router)
    self.maildir='spool'

  def sendMessage(self, frm, to, message):
    print('sendMessage '+str(frm)+' '+str(to)+' '+str(message))
    if not os.path.exists(self.maildir+'/'+frm):
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
