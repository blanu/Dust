from dust.core.data_packet import DataPacket
from dust.crypto.curve import Key
from dust.core.util import splitFields

SENDER_LENGTH=32
RECEIVER_LENGTH=32

class OnionPacket(DataPacket):
  def __init__(self):
    DataPacket.__init__(self)

    self.sender=None
    self.receiver=None

  def createOnionPacket(self, keypair, receiver, data, entropy):
    self.sender=keypair.public.bytes
    self.receiver=receiver
    sk=keypair.createSession(Key(self.receiver, False))
    self.createDataPacket(sk.bytes, data, entropy)
    self.packet=self.sender+self.receiver+self.packet

  def decodeOnionPacket(self, keypair, packet):
    self.sender, self.receiver, packet=splitFields(packet, [SENDER_LENGTH, RECEIVER_LENGTH])
    if keypair.public.bytes!=self.receiver:
      print('Error! Onion packet meant for a different receiver. Keypair does not match')
      return
    else:
      sk=keypair.createSession(Key(self.sender, False))
      self.decodeDataPacket(sk.bytes, packet)
