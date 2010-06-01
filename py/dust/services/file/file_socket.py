from dust.services.file.file_packet import FileMessage
from dust.extensions.multiplex.multiplex_socket import multiplex_socket

class file_socket(multiplex_socket):
  def __init__(self, keys):
    multiplex_socket.__init__(self, keys)
    self.connectToService('file')

  def fsend(self, headers, data):
    fmsg=FileMessage()
    fmsg.createFileMessage(headers, data)
    self.msend(fmsg.message, service='file')

  def fsendto(self, headers, data, addr):
    file=FileMessage()
    file.createFileMessage(headers, data)
    self.msendto(file.message, addr)

  def frecv(self):
    data, serviceName=self.mrecv(1024)
    if not data:
      return None, None
    file=FileMessage()
    file.decodeFileMessage(data)
    if serviceName!='file':
      print('Bad file service name', file.serviceName)
      return None, None
    return file.headers, file.data

  def frecvfrom(self):
    data, addr, serviceName=self.mrecvfrom(1024)
    if not data:
      print('File: No data')
      return None, None, None
    file=FileMessage()
    file.decodeFileMessage(data)
    if serviceName!='file':
      print('Bad file service name', file.serviceName)
      return None, None, None
    return file.headers, file.data, addr
