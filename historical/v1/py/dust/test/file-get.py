import sys

from dust.services.file.file_socket import file_socket
from dust.crypto.keys import KeyManager

from dust.server.services import services
print("services:", services)

host = '2002:ad82:39e1:7:250:8dff:fe5f:6e33'
inport=7001
outport=7000
passwd='test'

keys=KeyManager()
keys.setInvitePassword(passwd)
keys.loadKeypair('config/id.yaml')
keys.loadKnownHosts('config/knownhosts.yaml')
keys.loadIncomingInvites('config/incoming_invites.ip')
keys.loadOutgoingInvites('config/outgoing_invites.ip')

fsock=file_socket(keys)
fsock.bind((host, inport))
fsock.connect((host, outport))

headers={'command': 'get', 'file': 'test.txt', 'offset': 0}
data=None
fsock.fsend(headers, data)

headers, data, addr=fsock.frecvfrom()

print('got:', headers, data, addr)

#handler=services[service]
#print('Routing to', handler, '...')
#handler(msg, addr)