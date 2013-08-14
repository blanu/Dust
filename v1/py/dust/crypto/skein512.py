# Pure Python implementation of Skein-512-256.  Requires Python >=3.0.
# Written by Hagen, released to the public domain.

import sys
import traceback

from dust.core.util import encode

v3=(sys.version[0]=='3')
if v3:
  empty=bytes('', 'ascii')
else:
  empty=''

### bytes <--> words conversions ###

def BytesToWords(b, n):
    """Return n words for 8*n bytes."""
    result=[]
    for i in range(n):
      scratch=[]
      for j in range(8):
        if v3:
          scratch.append(b[8*i+j]<<(8*j))
        else:
          scratch.append(ord(b[8*i+j])<<(8*j))
      result.append(sum(scratch))
    return result

def WordsToBytes(w):
    """Return 8*n bytes for n words."""
    scratch=[(v>>(8*j))&255 for v in w for j in range(8)]
    if v3:
      return bytearray(scratch)
    else:
      return ''.join(map(chr, scratch))


### MIX function and inverse ###

R = [[46, 36, 19, 37],
     [33, 27, 14, 42],
     [17, 49, 36, 39],
     [44, 9, 54, 56],
     [39, 30, 34, 24],
     [13, 50, 10, 17],
     [25, 29, 39, 43],
     [8, 35, 56, 22]]

def mix(d, j, x0, x1):
    r = R[d%8][j]
    y0 = (x0+x1) & (2**64-1)
    y1 = ((x1<<r) | (x1>>(64-r))) & (2**64-1)
    y1 ^= y0
    return y0, y1

def mix_inv(d, j, y0, y1):
    r = R[d%8][j]
    y1 ^= y0
    x1 = (y1>>r) | (y1<<(64-r)) & (2**64-1)
    x0 = (y0-x1) & (2**64-1)
    return x0, x1


### Threefish ###

def subkeys(k, t):
    ek = 2**64//3
    for kw in k:
        ek ^= kw
    k.append(ek)
    t.append(t[0]^t[1])
    for s in range(19):
        sk = [k[(s+i)%9] for i in range(5)]
        sk.append((k[(s+5)%9]+t[s%3]) & (2**64-1))
        sk.append((k[(s+6)%9]+t[(s+1)%3]) & (2**64-1))
        sk.append((k[(s+7)%9]+s) & (2**64-1))
        yield sk

PI = [2, 1, 4, 7, 6, 5, 0, 3]
def threefish(key, tweak, plain):
    """'key' and 'plain' contain 64 bytes, 'tweak' contains 16 bytes."""
#    print('threefish('+encode(key)+', '+encode(tweak)+', '+encode(plain)+')')
    k = BytesToWords(key, 8)
    t = BytesToWords(tweak, 2)
    v = BytesToWords(plain, 8)
    f = [0]*8
    d = 0
    for sk in subkeys(k, t):
        for i in range(8):
            v[i] = (v[i]+sk[i]) & (2**64-1)
        if d == 72:
            break
        for _ in range(4):
            for j in range(4):
                f[2*j], f[2*j+1] = mix(d, j, v[2*j], v[2*j+1])
            for i in range(8):
                v[i] = f[PI[i]]
            d += 1
    return WordsToBytes(v)

def threefish_decrypt(key, tweak, encrypted):
    """'key' and 'plain' contain 64 bytes, 'tweak' contains 16 bytes."""
    k = BytesToWords(key, 8)
    t = BytesToWords(tweak, 2)
    v = BytesToWords(encrypted, 8)
    f = [0]*8
    d = 72
    for sk in reversed(list(subkeys(k, t))):
        for i in range(8):
            v[i] = (v[i]-sk[i]) & (2**64-1)
        if d == 0:
            break
        for _ in range(4):
            d -= 1
            for i in range(8):
                f[PI[i]] = v[i]
            for j in range(4):
                v[2*j], v[2*j+1] = mix_inv(d, j, f[2*j], f[2*j+1])
    return WordsToBytes(v)


### Skein ###

def ubi(g, m, ts):
    #print('ubi('+encode(g)+', '+encode(m)+', '+str(ts)+')')
    if v3:
      m = bytearray(m)
    else:
      m = str(m)
    l = len(m)
    if (l == 0) or (l%64 != 0):
      if v3:
        m.extend([0]*(64-l%64))
      else:
        m=m+("\x00"*(64-l%64))
    if v3:
      h = bytearray(g)
    else:
      h = str(g)
    ts_pos = ts
    for i in range(0, len(m), 64):
        block = m[i:i+64]
        ts_pos += 64
        tweak = ts_pos
        if i == 0:
            tweak |= 1<<126
        if i == len(m)-64:
            tweak |= 1<<127
            tweak -= len(m)-l
        tweak_bytes = WordsToBytes([tweak&(2**64-1), tweak>>64])
        cipher = threefish(h, tweak_bytes, block)
        if v3:
          h = bytearray(x^y for x, y in zip(cipher, block))
        else:
          h = ''.join([chr(ord(x)^ord(y)) for x, y in zip(cipher, block)])
    return h

def skein512(msg=None, mac=None, pers=None, nonce=None, tree=None, digest_bits=512):
    if digest_bits==512:
      if v3:
        CONFIG = bytes("SHA3", 'ascii')+bytes("\1\0", 'ascii')+bytes("\0\0", 'ascii')+bytes("\0\2\0\0\0\0\0\0", 'ascii')
      else:
        CONFIG = "SHA3"+"\1\0"+"\0\0"+"\0\2\0\0\0\0\0\0"
    elif digest_bits==256:
      if v3:
        CONFIG = bytes("SHA3", 'ascii')+bytes("\1\0", 'ascii')+bytes("\0\0", 'ascii')+bytes("\0\1\0\0\0\0\0\0", 'ascii')
      else:
        CONFIG = "SHA3"+"\1\0"+"\0\0"+"\0\1\0\0\0\0\0\0"
    else:
      print('digest_bits must be 512 or 256')
      return None

    tree_leaf, tree_fan, tree_max = tree if (tree is not None) else (0, 0, 0)

    if v3:
      g = bytes(64)
    else:
      g = "\x00"*64

    if mac:
      g = ubi(g, mac, 0)

    if v3:
      config = CONFIG + bytes((tree_leaf, tree_fan, tree_max)) + bytes(13)
    else:
      config = CONFIG + chr(tree_leaf)+chr(tree_fan)+chr(tree_max) + "\x00"*13

    g = ubi(g, config, 4<<120)
    if pers:
        g = ubi(g, pers, 8<<120)
    if nonce:
        g = ubi(g, nonce, 20<<120)
    if msg and (tree_leaf == tree_fan == tree_max == 0):
        g = ubi(g, msg, 48<<120)
    else:
        if tree_leaf < 1:
            raise ValueError("tree leaf parameter has to be >= 1")
        if tree_fan < 1:
            raise ValueError("tree fan-out parameter has to be >= 1")
        if tree_max < 2:
            raise ValueError("maximum tree depth parameter has to be >= 2")
        g = tree_hash(g, msg, 64*(2**tree_leaf), 2**tree_fan, tree_max)

    if digest_bits==512:
      if v3:
        g = ubi(g, bytes(8), 63<<120)
      else:
        g = ubi(g, "\x00"*8, 63<<120)
    else:
      if v3:
        g = ubi(g, bytes(8), 63<<120)[:32]
      else:
        g = ubi(g, "\x00"*8, 63<<120)[:32]

    return g

def tree_hash(g, msg, leaf_size, children, max_level):
    if msg:
        blocks = [msg[i*leaf_size:(i+1)*leaf_size]
                  for i in range((len(msg)-1)//leaf_size+1)]
    else:
        blocks = [empty]
    blocks = [ubi(g, block, (i*leaf_size)|(1<<112)|(48<<120))
              for i, block in enumerate(blocks)]
    level = 2
    while (level < max_level) and (len(blocks) > 1):
        blocks = [empty.join(blocks[i*children:(i+1)*children])
                  for i in range((len(blocks)-1)//children+1)]
        blocks = [ubi(g, block, (64*i*children)|(level<<112)|(48<<120))
                  for i, block in enumerate(blocks)]
        level += 1
    if len(blocks) == 1:
        return blocks[0]
    else:
        return ubi(g, empty.join(blocks), (max_level<<112)|(48<<120))

