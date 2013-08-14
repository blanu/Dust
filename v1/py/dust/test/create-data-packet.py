from dust.crypto.keys import KeyManager
from dust.core.data_packet import DataPacket
from dust.core.util import encode, decode

keys=KeyManager()
#psk=keys.entropy.getBytes(32)

psk=decode("5475e69147a1463ef65116ccd8b3d732ead5ce8b5c9b0e61eb4c218fe6165013")

packet=DataPacket()
packet.createDataPacket(psk, b"test #3", keys.entropy)
print('packet:', packet)
print('packetData:', encode(packet.packet))

print('------------------------')

p2=DataPacket()
p2.decodeDataPacket(psk, packet.packet)
print(p2)
print('checkMac:', p2.checkMac())
print('checkTimestamp:', p2.checkTimestamp())