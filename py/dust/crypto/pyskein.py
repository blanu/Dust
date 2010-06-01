try:
  from skein import skein512 as cskein512

  def skein512(msg=b"", mac=b"", pers=b"", nonce=b"", tree=None, digest_bits=512):
    return cskein512(msg, mac=mac, pers=pers, nonce=nonce, tree=tree, digest_bits=digest_bits).digest()
#  raise(Exception())
except:
  print('Using pure python skein')
  from dust.crypto.skein512_512 import skein512_512
  from dust.crypto.skein512_256 import skein512_256

  def skein512(msg=b"", mac=b"", pers=b"", nonce=b"", tree=None, digest_bits=512):
    if digest_bits==512:
      return skein512_512(msg, mac, pers, nonce, tree)
    elif digest_bits==256:
      return skein512_256(msg, mac, pers, nonce, tree)
    else:
      print('Digest_bits must be 512 or 256')
      return None

