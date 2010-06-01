from dust.crypto.keys import KeyManager
from dust.core.data_packet import DataPacket

keys=KeyManager()
psk=keys.entropy.getBytes(32)
  
packet=DataPacket()
packet.createDataPacket(psk, b"Hello", keys.entropy)
print(packet)
print('l:', len(packet.packet))

print('------------------------')

pc=DataPacket()
pc.createDataPacket(psk, b"Chained!", keys.entropy)
print('l:', len(pc.packet))

print(pc)

chain=packet.packet+pc.packet

print('l:', len(chain))

print('------------------------')

p2=DataPacket()
p2.decodeDataPacket(psk, chain)
print(p2)

print('------------------------')

pc2=DataPacket()
pc2.decodeDataPacket(psk, p2.remaining)
print(pc2)
