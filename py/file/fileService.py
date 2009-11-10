import os
import yaml
import json

from file.file_message import FileMessage

class FileHandler:
  def __init__(self):
    f=open('config/fileService.yaml', 'r')
    self.config=yaml.load(f.read())
    f.close()

    self.commands={'getMeta': self.getMeta, 'putMeta': self.putMeta, 'get': self.get, 'put': self.put}
    self.blockSize=1024

  def handle(self, msock, msg, addr):
    print('-----------------')
    print(msg.decode('ascii'))

    fmsg=FileMessage()
    fmsg.decodeFileMessage(msg)
    cmd=fmsg.headers['command']
    if cmd in self.commands:
      f=self.commands[cmd]
      f(msock, fmsg.headers, fmsg.data, addr)
    else:
      print('Unknown file command', cmd)
      return

  def getMeta(self, msock, headers, data, addr):
    file=headers['file']

    result={'command': 'putMeta', 'file': file}
    result['length']=os.path.getsize(file)

    fmsg=FileMessage()
    fmsg.createFileMessage(result, None)
    msock.sendto(fmsg.message, addr, service='file')

  def putMeta(self, msock, headers, data, addr):
    file=headers['file']
    length=headers['length']

    print('got meta:', file, length)

  def get(self, msock, headers, data, addr):
    file=headers['file']
    offset=headers['offset']

    f=open(file, 'rb')
    f.seek(offset)
    bs=f.read(self.blockSize)

    result={'command': 'put', 'file': file, 'offset': offset}

    fmsg=FileMessage()
    fmsg.createFileMessage(result, bs)

    msock.sendto(fmsg.message, addr, service='filedata')

  def put(self, msock, headers, data, addr):
    file=headers['file']
    offset=headers['offset']

    print('got put', file, offset, len(data))