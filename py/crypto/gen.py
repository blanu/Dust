import sys
from crypto.curve import createKeypair
from crypto.keys import KeyManager

keys=KeyManager()
keys.keypair = createKeypair()
keys.saveKeypair('config/id.yaml')
