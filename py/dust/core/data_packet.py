from dust.core.dust_packet import DustPacket

class DataPacket(DustPacket):
  def __init__(self):
    DustPacket.__init__(self)

  def createDataPacket(self, key, data, entropy):
    self.createDustPacket(key, data, entropy)

  def decodeDataPacket(self, key, packet):
    self.decodeDustPacket(key, packet)













































