import sys

v3=(sys.version[0]=='3')

try:
  from skein import skein512 as cskein512

  def skein512(msg=None, mac=None, pers=None, nonce=None, tree=None, digest_bits=512):
    if msg==None:
      msg=bytes('', 'ascii')
    if mac==None:
      mac=bytes('', 'ascii')
    if pers==None:
      pers=bytes('', 'ascii')
    if nonce==None:
      nonce=bytes('', 'ascii')
    return cskein512(msg, mac=mac, pers=pers, nonce=nonce, tree=tree, digest_bits=digest_bits).digest()
#  raise(Exception())
except:
  #print('Using pure python skein')
  from dust.crypto.skein512 import skein512
