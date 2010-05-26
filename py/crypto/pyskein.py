try:
  from skein import skein512
except:
  from crypto.skein512_512 import skein512_512
  from crypto.skein512_256 import skein512_256

  def skein512(msg, mac=b"", pers=b"", nonce=b"", tree=None, digest_bits=512):
    if digest_bits==512:
      return skein512_512(msg, mac, pers, nonce, tree)
    elif digest_bits==256:
      return skein512_256(msg, mac, pers, nonce, tree)
    else:
      print('Digest_bits must be 512 or 256')
      return None