import sys

from dust.server.lite_router import LitePacketRouter
from dust.crypto.keys import KeyManager

inport=int(sys.argv[1])
v6=sys.argv[2]

keys=KeyManager()

LitePacketRouter(v6, inport, keys)

