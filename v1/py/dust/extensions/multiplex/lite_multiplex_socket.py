from dust.extensions.multiplex.multiplex_packet import MultiplexMessage
from dust.extensions.lite.lite_socket import lite_socket

class lite_multiplex_socket(lite_socket):
  def __init__(self, keys):
    lite_socket.__init__(self, keys)
    
    self.connectService=None
    
  def connectToService(self, serviceName):
    self.connectService=serviceName
    
  def msend(self, data, service=None):
    if not service:
      service=self.connectService
    multiplex=MultiplexMessage()
    multiplex.createMultiplexMessage(service, data)
    lite_socket.send(self, multiplex.message)
    
  def msendto(self, data, addr, service=None):
    if not service:
      service=self.connectService
    multiplex=MultiplexMessage()
    multiplex.createMultiplexMessage(service, data)
    list_socket.sendto(self, multiplex.message, addr)    
    
  def mrecv(self, bufsize, service=None):
    if not service:
      service=self.connectService
    data=lite_socket.recv(self, bufsize)
    if not data:
      return None
    multiplex=MultiplexMessage()
    multiplex.decodeMultiplexMessage(data)
    if service and multiplex.serviceName!=service:
      print('Bad multiplex service name', multiplex.serviceName, 'should be ', self.connectService)
      return None
    return multiplex.data

  def mrecvfrom(self, bufsize, service=None):
    if not service:
      service=self.connectService
    data, addr=lite_socket.recvfrom(self, bufsize)
    if not data:
      print('No data')
      return None, None, None
    multiplex=MultiplexMessage()
    multiplex.decodeMultiplexMessage(data)
    if service and multiplex.serviceName!=service:
      print('Bad multiplex service name', multiplex.serviceName, 'should be ', self.connectService)
      return None, None, None
    return multiplex.data, addr, multiplex.serviceName
    