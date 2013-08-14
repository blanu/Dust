#from dust.crypto.dust import DustPRNG, encrypt, decrypt
from dust.core.util import encode, decode
from dust.crypto.pureskein512 import skein512 as pure
from skein import skein512

#r=DustPRNG()
#iv=r.getBytes(16)
#key=r.getBytes(32)
key=decode("5475e69147a1463ef65116ccd8b3d732ead5ce8b5c9b0e61eb4c218fe6165013")
iv=decode("5475e69147a1463ef65116ccd8b3d732ead5ce8b5c9b0e61eb4c218fe6165013")

text=b'test data'

def purehash(data, pers=None):
  if pers:
    return pure(data, pers=pers)
  else:
    return pure(data)

def chash(data, pers=None):
  if pers:
    return skein512(data, pers=pers).digest()
  else:
    return skein512(data).digest()

print("hash: "+encode(purehash(text)))
print("hash pers: "+encode(purehash(text, pers=b"a")))
print("hash: "+encode(chash(text)))
print("hash pers: "+encode(chash(text, pers=b"a")))

#data=encrypt(key, iv, text)
#print(text)
#print(encode(data))
#print(decrypt(key, iv, data))
