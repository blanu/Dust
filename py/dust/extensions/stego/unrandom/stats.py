import sys

f=open(sys.argv[1], 'rb')
bs=f.read()
f.close()

counts=[]
for x in range(256):
  counts.append(0)

for b in bs:
  i=ord(b)
  counts[i]=counts[i]+1

print(counts)

for x in range(len(counts)):
  counts[x]=float(counts[x])/len(bs)

print(counts)

f=open(sys.argv[2], 'w')
for count in counts:
  f.write(str(count)+"\n")
f.close()
