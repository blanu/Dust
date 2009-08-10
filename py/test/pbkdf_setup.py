from crypto import skeinUtil

passwd="testpw"
prng=skeinUtil.SkeinPRNG()
salt=prng.getBytes(32)
pbkdf=skeinUtil.pbkdf