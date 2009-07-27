from multiplex.multiplex_socket import *
from crypto.keys import KeyManager
from core.util import getPublicIP

from server.services import services
print("services:", services)

host = getPublicIP()
inport=7000

keys=KeyManager()
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadKeypair('config/id.yaml')

msock=multiplex_socket(keys)
msock.bind((host, inport))

while True:
  msg, addr, service=msock.mrecvfrom(1024)
  if msg and addr and service:
    handler=services[service]
    print('Routing to', handler, '...')
    handler(msg, addr)