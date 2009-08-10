import timeit

from crypto import skeinUtil

passwd="testpw"

prng=skeinUtil.SkeinPRNG()
salt=prng.getBytes(32)

setup="import pbkdf_setup"

stmt="""
  print(pbkdf_setup.pbkdf(pbkdf_setup.passwd, pbkdf_setup.salt, 13000))
"""

r=10

t=timeit.Timer(stmt, setup)
try:
  print(t.timeit(number=r)/r)
except:
  t.print_exc()