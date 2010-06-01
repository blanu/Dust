#from dust.crypto.dust import DustPRNG, encrypt, decrypt
from dust.core.util import encode, decode
from dust.crypto.skeinUtil import encrypt, decrypt, hash

#r=DustPRNG()
#iv=r.getBytes(16)
#key=r.getBytes(32)
key=decode("5475e69147a1463ef65116ccd8b3d732ead5ce8b5c9b0e61eb4c218fe6165013")
iv=decode("5475e69147a1463ef65116ccd8b3d732ead5ce8b5c9b0e61eb4c218fe6165013")

text=b'test data'

print("hash: "+encode(hash(text)))
print("hash pers: "+encode(hash(text, pers=b"a")))

#data=encrypt(key, iv, text)
#print(text)
#print(encode(data))
#print(decrypt(key, iv, data))
