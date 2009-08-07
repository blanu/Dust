from multiplex.multiplex_socket import *
from crypto.keys import KeyManager
from core.util import getPublicIP

from server.services import services
print("services:", services)

class PacketRouter:
  def __init__(self, v6, port, keys, passwd):
    self.host = getPublicIP(v6)
    self.port=port
    self.keys=keys
    self.passwd=passwd

    msock=multiplex_socket(self.keys)
    msock.bind((self.host, self.port))

    while True:
      msg, addr, service=msock.mrecvfrom(1024)
      if msg and addr and service:
        handler=services[service]
        print('Routing to', handler, '...')
        handler(msg, addr)