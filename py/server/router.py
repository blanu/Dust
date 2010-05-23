from extensions.multiplex.multiplex_socket import *
from crypto.keys import KeyManager
from core.util import getPublicIP
from util.safethread import SafeThread

from server.activeServices import activeServices
print("services:", activeServices)

class PacketRouter:
  def __init__(self, v6, port, keys, passwd):
    self.host = getPublicIP(v6)
    self.port=port
    self.keys=keys
    self.passwd=passwd

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
    for service in services.values():
      service.setUI(ui)

  def start(self):
    self.thread=SafeThread(target=self.run)
    self.thread.start()

  def run(self):
    while True:
      msg, addr, service=self.msock.mrecvfrom(1024)
      if msg and addr and service:
        handler=activeServices[service]
#        print('Routing to', handler, '...')
        handler.handle(self.msock, msg, addr)

  def send(self, msg, service=None):
    if service:
      self.msock.msend(msg.encode('ascii'), service=service)
    else:
      self.msock.msend(msg.encode('ascii'))

  def sendto(self, msg, addr, service=None):
    print('router.sendto '+str(addr))
    if service:
      self.msock.msendto(msg.encode('ascii'), addr, service=service)
    else:
      self.msock.msendto(msg.encode('ascii'), addr)
