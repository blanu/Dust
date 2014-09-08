import re
import sys
import binascii

from struct import pack, unpack
from socket import inet_aton, inet_ntoa

v3=(sys.version[0]=='3')

def uncompact(x):
    ip, port = unpack("!4sH", x)
    return inet_ntoa(ip), port

def encode(s):
  return binascii.hexlify(s).decode('ascii')

def decode(s):
  return binascii.unhexlify(s.encode('ascii'))

def encodeAddress(addr):
  ip=addr[0]
  if ip=='':
    ip='::'
  port=addr[1]
  if '.' in ip:
    return ip+':'+str(port)
  else:
    return '['+ip+']:'+str(port)

def decodeAddress(s):
  if '.' in s:
    parts=s.split(':')
    return (parts[0], int(parts[1]), False)
  else:
    m=re.match('\[([0-9a-f:]+)\]:([0-9]+)', s)
    return (m.group(1), int(m.group(2)), True)

def getAddress(port):
  return encodeAddress((getPublicIP(), port))

def splitFields(msg, fields, optionalData=False):
  values=[]
  for field in fields:
    value=msg[:field]
    msg=msg[field:]
    values.append(value)
  if len(msg)>0:
    values.append(msg)
  elif optionalData:
    values.append(None)
  return values

def splitField(msg, field):
  return msg[:field], msg[field:]

def decodeFlags(flagsByte):
  from bitstring import BitString
  bits=BitString(bytes=flagsByte)
  bools=[]
  for x in range(bits.length):
    bools.append(bits.readbit().uint==1)
  return bools

def encodeFlags(bools):
  from bitstring import BitString
  bits=BitString()
  for bool in bools:
    if bool:
      bits.append(BitString('0b1'))
    else:
      bits.append(BitString('0b0'))
  return bits.bytes

def fill(bs, size):
  while len(bs)<size:
    if v3:
      filler=bytes('\x00', 'ascii')
    else:
      filler='\x00'
    bs=bs+filler
  return bs

def xor(a, b):
  if len(a)!=len(b):
    print('xor parameters must be the same length:', len(a), len(b))
    return None
  if v3:
    c=bytearray()
    for x in range(len(a)):
      c.append(a[x] ^ b[x])
    return bytes(c)
  else:
    c=''
    for x in range(len(a)):
      c=c+chr(ord(a[x]) ^ ord(b[x]))
    return c

if v3:
  from urllib.request import urlopen
else:
  from urllib2 import urlopen

def getPublicIP(v6=True):
  if v6:
#    try:
      text=urlopen("http://ipv6.ip6.me/").read()
      if v3:
        match=re.search(bytes("\+3>([^<]+)<", 'ascii'), text)
      else:
        match=re.search("\+3>([^<]+)<", text)
      ip=match.group(1)
      ip=ip.decode('ascii')
      return ip
#    except Exception as e:
#      print(e)
#      ip=urlopen("http://whatismyv6ip.com/myip").read()
#      return ip.decode('ascii')
  else:
    text=urlopen("http://ip4.me/").read()
    if v3:
      match=re.search(bytes("\+3>([^<]+)<", 'ascii'), text)
    else:
      match=re.search("\+3>([^<]+)<", text)
#     ip=urlopen("http://whatismyv6ip.com/myip").read()
#     return ip.decode('ascii')
    ip=match.group(1)
    ip=ip.decode('ascii')
    return ip

def randomPort():
  import random
  minPort=5000
  maxPort=10000
  port=random.randint(minPort, maxPort)
  return port
