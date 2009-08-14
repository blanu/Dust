from crypto.dust import DustPRNG, encrypt, decrypt

r=DustPRNG()
iv=r.getBytes(16)
key=r.getBytes(32)

text=b'Hello'

data=encrypt(key, iv, text)
print(data)
print(decrypt(key, iv, data))
