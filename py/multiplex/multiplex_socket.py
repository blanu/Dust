from multiplex.multiplex_packet import MultiplexMessage
from core.ec_socket import ec_socket

class multiplex_socket(ec_socket):
  def __init__(self, keys):
    ec_socket.__init__(self, keys)
    
    self.connectService=None
    
  def connectToService(self, serviceName):
    self.connectService=serviceName
    
  def msend(self, data, service=None):
    print('ss:', service)
    if not service:
      service=self.connectService
    multiplex=MultiplexMessage()
    multiplex.createMultiplexMessage(service, data)
    ec_socket.send(self, multiplex.message)
    
  def msendto(self, data, addr, service=None):
    if not service:
      service=self.connectService
    multiplex=MultiplexMessage()
    multiplex.createMultiplexMessage(service, data)
    ec_socket.sendto(self, multiplex.message, addr)    
    
  def mrecv(self, bufsize, service=None):
    if not service:
      service=self.connectService
    data=ec_socket.recv(self, bufsize)
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
    data, addr=ec_socket.recvfrom(self, bufsize)
    if not data:
      print('No data')
      return None, None, None
    multiplex=MultiplexMessage()
    multiplex.decodeMultiplexMessage(data)
    if service and multiplex.serviceName!=service:
      print('Bad multiplex service name', multiplex.serviceName, 'should be ', self.connectService)
      return None, None, None
    return multiplex.data, addr, multiplex.serviceName
    