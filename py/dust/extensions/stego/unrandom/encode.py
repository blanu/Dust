import sys

from encoder import Encoder

f=open('stats.out')
lines=f.readlines()
f.close()

dist=[]
for x in range(len(lines)):
  p=float(lines[x].strip())
  dist.append((p, x))

enc=Encoder(dist)
enc.encode(sys.argv[1], sys.argv[2])
enc.decode(sys.argv[2], sys.argv[3])


