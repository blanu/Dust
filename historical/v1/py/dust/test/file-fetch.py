import sys

from dust.services.file.file_socket import file_socket
from dust.crypto.keys import KeyManager
from dust.services.file.fileService import FileHandler

host = '2002:ae94:84fa:4:250:8dff:fe5f:6e33'
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

headers={'command': 'getMeta', 'file': 'test.txt'}
fsock.fsend(headers, None)
headers, data, addr=fsock.frecvfrom()
command=headers['command']

if command=='putMeta':
  l=headers['length']
else:
  print('Expecting putMeta, got', command)

print('downloading', l, 'bytes')

file='test.txt'
f=open('incoming/'+file, 'wb')
written=0
while written<l:
  headers={'command': 'get', 'file': 'test.txt', 'offset': written}
  fsock.fsend(headers, None)

  headers, data, addr=fsock.frecvfrom()
  if headers:
    command=headers['command']
    if command=='put':
      filename=headers['file']
      offset=headers['offset']

      if file!=filename:
        print('Wrong filename', filename, 'should be', file)
      else:
        print('writing', len(data), 'to', offset)
        f.seek(offset)
        f.write(data)
        written=written+len(data)
    else:
      print('Expecting put, got', command)
  else:
    print('No headers')
f.close()
print('Done!')
