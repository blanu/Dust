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

pers=b'1978-10-26 dust@blanu.net Dust/cipher'

cp=SkeinCipher(key, iv, pers=pers)
d3=cp.encrypt(text)
print(d3)

cp2=SkeinCipher(key, iv, pers=pers)
d4=cp2.decrypt(d3)
print(d4)
