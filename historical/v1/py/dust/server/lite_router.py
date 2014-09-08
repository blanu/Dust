from dust.extensions.multiplex.lite_multiplex_socket import *
from dust.crypto.keys import KeyManager
from dust.core.util import getPublicIP

from dust.server.services import services
print("services:", services)

class LitePacketRouter:
  def __init__(self, v6, port, keys):
    self.host = getPublicIP(v6)
    self.port=port

    msock=lite_multiplex_socket(keys)
    msock.bind((self.host, self.port))

    while True:
      msg, addr, service=msock.mrecvfrom(1024)
      if msg and addr and service:
        handler=services[service]
#        print('Routing to', handler, '...')
        handler(msg, addr)