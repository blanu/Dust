import timeit

from crypto import skeinUtil

passwd="testpw"

prng=skeinUtil.SkeinPRNG()
salt=prng.getBytes(32)

setup="import pbkdf_setup"

stmt="""
  print(pbkdf_setup.pbkdf(pbkdf_setup.passwd, pbkdf_setup.salt, 13000))
"""

stmt2="""
  print(pbkdf_setup.pbkdf(pbkdf_setup.passwd, pbkdf_setup.salt, 13000, pers=pbkdf_setup.pers))
"""

r=10

t=timeit.Timer(stmt, setup)
try:
  print(t.timeit(number=r)/r)
except:
  t.print_exc()
  
t2=timeit.Timer(stmt2, setup)
try:
  print(t2.timeit(number=r)/r)
except:
  t2.print_exc()
  