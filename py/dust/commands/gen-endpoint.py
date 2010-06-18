#!/usr/bin/python
import os
import sys
# python sucks
sys.path.insert(0, os.path.realpath(os.path.join(os.path.dirname(__file__), "..")))

from dust.crypto.keys import KeyManager

keys=KeyManager()
keys.createKeypair()

dustdir=os.path.expanduser("~/.dust")
if not os.path.exists(dustdir):
  os.mkdir(dustdir)
keys.saveKeypair(dustdir+'/endpoint.yaml')
