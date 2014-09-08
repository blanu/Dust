import logging
from dust.core.data_packet import DataPacket
from dust.core.util import splitFields, encode

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
    skb=keypair.createSessionBytes(self.receiver)
    self.createDataPacket(skb, data, entropy)
    self.packet=self.sender+self.receiver+self.packet

  def decodeOnionPacket(self, keypair, packet):
    self.sender, self.receiver, packet=splitFields(packet, [SENDER_LENGTH, RECEIVER_LENGTH])
    if keypair.public.bytes!=self.receiver:
      print('Error! Onion packet meant for a different receiver. Keypair does not match: '+encode(self.sender)+' '+encode(self.receiver)+' '+encode(keypair.public.bytes))
      return
    else:
      skb=keypair.createSessionBytes(self.sender)
      self.decodeDataPacket(skb, packet)
