from dust.extensions.multiplex.multiplex_socket import *
from dust.crypto.keys import KeyManager
from dust.core.util import getPublicIP
from dust.util.safethread import SafeThread

from dust.server.activeServices import activeServices
#print("services:", list(activeServices.keys()))

class PacketRouter:
  def __init__(self, v6, port, keys, passwd, debug=False):
    self.v6=v6
    self.host = getPublicIP(v6)
    self.port=port
    self.keys=keys
    self.passwd=passwd
    self.tcp=True
    self.debug=debug

    self.msock=multiplex_socket(self.keys)
    self.msock.bind((self.host, self.port))

    self.ui=None

    for service in activeServices.values():
      service.setRouter(self)

  def getService(self, name):
    return activeServices[name]

  def connect(self, dest, outport):
    self.msock.connect((dest, outport))

  def connectToService(self, service):
    self.msock.connectToService(service)

  def setUI(self, ui):
    self.ui=ui
    for service in activeServices.values():
      service.setUI(ui)

  def start(self):
    self.thread=SafeThread(target=self.run)
    self.thread.setDaemon(True)
    self.thread.start()
    return self.thread

  def run(self):
    while True:
      msg, addr, service=self.msock.mrecvfrom(1024)
      if self.debug:
        print('Received from '+str(addr)+' '+str(service))
      if msg and addr and service:
        handler=activeServices[service]
#        print('Routing to', handler, '...')
        handler.handle(self.msock, msg, addr)

  def send(self, msg, service=None):
    if service:
      self.msock.msend(msg, service=service)
    else:
      self.msock.msend(msg)

  def sendto(self, msg, addr, service=None):
    if self.debug:
      print('router.sendto '+str(addr)+' '+str(service))
    if service:
      self.msock.msendto(msg, addr, service=service)
    else:
      self.msock.msendto(msg, addr)

  def generateInvite(self):
    return self.keys.generateInvite(self.port, v6=self.v6, tcp=self.tcp)
