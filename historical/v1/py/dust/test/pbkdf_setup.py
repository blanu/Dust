from dust.crypto import dust

passwd="testpw"
prng=dust.DustPRNG()
salt=prng.getBytes(32)
pbkdf=dust.pbkdf