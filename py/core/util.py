import re
import binascii
from urllib.request import urlopen

def encode(s):
  return binascii.hexlify(s).decode('ascii')
  
def decode(s):
  return binascii.unhexlify(s.encode('ascii'))
  
def encodeAddress(addr):
  ip=addr[0]
  if ip=='':
    ip='::'
  port=addr[1]
  return '['+ip+']:'+str(port)
  
def decodeAddress(s):
  print('decodeAddress:', s)
  m=re.match('\[([0-9a-f:]+)\]:([0-9]+)', s)
  print('m:', m)
  return (m.group(1), int(m.group(2)))
  
def getPublicIP():
#  text=urlopen("http://ip6.me/").read()
#match=re.search(b"\+3>([^<]+)<", text)
  text=urlopen("http://whatismyv6ip.com/").read()
  match=re.search(b"<h2>Your global IP address is: ([^<]+)<", text)
  ip=match.group(1)
  return ip.decode('ascii')

def getAddress(port):
  return encodeAddress((getPublicIP(), port))
