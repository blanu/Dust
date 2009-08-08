from crypto.skeinUtil import SkeinPRNG

r=SkeinPRNG()
print(r.getBytes(10))
print(r.getBytes(20))
print(r.getBytes(1))

print(r.getInt())
print(r.getInt())
print(r.getInt())
print(r.getInt())

print(r.getInt(255))
print(r.getInt(255))
print(r.getInt(255))
print(r.getInt(255))
