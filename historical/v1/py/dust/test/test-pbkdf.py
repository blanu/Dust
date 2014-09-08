import timeit

from dust.crypto.dust import DustPRNG

passwd="testpw"

prng=DustPRNG()
salt=prng.getBytes(32)

setup="import pbkdf_setup"

stmt="""
  print(pbkdf_setup.pbkdf(pbkdf_setup.passwd, pbkdf_setup.salt))
"""

r=10

t=timeit.Timer(stmt, setup)
try:
  print(t.timeit(number=r)/r)
except:
  t.print_exc()  