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
  print('Using pure python skein')
  from dust.crypto.skein512_512 import skein512_512
  from dust.crypto.skein512_256 import skein512_256

  def skein512(msg=None, mac=None, pers=None, nonce=None, tree=None, digest_bits=512):
    if msg==None:
      if v3:
        msg=bytes('', 'ascii')
      else:
        msg=''
    if mac==None:
      if v3:
        mac=bytes('', 'ascii')
      else:
        msg=''
    if pers==None:
      if v3:
        pers=bytes('', 'ascii')
      else:
        msg=''
    if nonce==None:
      if v3:
        nonce=bytes('', 'ascii')
      else:
        msg=''
    if digest_bits==512:
      return skein512_512(msg, mac, pers, nonce, tree)
    elif digest_bits==256:
      return skein512_256(msg, mac, pers, nonce, tree)
    else:
      print('Digest_bits must be 512 or 256')
      return None
