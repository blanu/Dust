from crypto import skeinUtil

passwd="testpw"
prng=skeinUtil.SkeinPRNG()
salt=prng.getBytes(32)
pbkdf=skeinUtil.pbkdf
pers=b'1978-10-26 dust@blanu.net Dust/PBKDF'