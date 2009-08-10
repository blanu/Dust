from crypto.keys import KeyManager
from core.data_packet import DataPacket

keys=KeyManager()
psk=keys.entropy.getBytes(32)
  
packet=DataPacket()
packet.createDataPacket(psk, b"Hello", keys.entropy)
print('packet:', packet)
print('packetData:', packet.packet)
print('length', packet.length)
print('checkMac:', packet.checkMac())
print('packet length:', len(packet.packet))

print('------------------------')

p2=DataPacket()
p2.decodeDataPacket(psk, packet.packet)
print(p2)
print('checkMac:', p2.checkMac())
print('checkTimestamp:', p2.checkTimestamp())