import os
import sys
# python sucks
sys.path.insert(0, os.path.realpath(os.path.join(os.path.dirname(__file__), "..")))

from crypto.keys import KeyManager

keys=KeyManager()
keys.createKeypair()
keys.saveKeypair('config/id.yaml')
