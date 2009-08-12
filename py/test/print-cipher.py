from crypto.skeinUtil import SkeinPRNG, SkeinCipher

r=SkeinPRNG()
iv=r.getBytes(16)
key=r.getBytes(32)

text=b'Hello'

cipher=SkeinCipher(key, iv)
data=cipher.encrypt(text)
print(data)

c2=SkeinCipher(key, iv)
d2=c2.decrypt(data)
print(d2)
