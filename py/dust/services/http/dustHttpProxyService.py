import bz2
import math
import time
import urllib.request
from dust.core.util import encodeAddress
from dust.services.http.proxyback_packet import ProxybackMessage
from dust.services.http.proxy_packet import ProxyMessage

class DustHttpProxyService:
  def __init__(self):
    self.router=None

  def setRouter(self, r):
    self.router=r;

  def handle(self, msock, msg, addr):
    packet=ProxyMessage()
    packet.decodeProxyMessage(msg)
    reqid=packet.reqid
    data=packet.data.decode('ascii')
    print('HTTP Proxy message from '+encodeAddress(addr)+':')
    print(data)
    parts=data.split(' ')
    print('parts: '+str(parts))
    url=parts[1]
    print('url: '+str(url))
    resp=urllib.request.urlopen(url)
    result=resp.read()
    print('result: '+str(result)+' '+str(len(result)))
#    comp=bz2.compress(result)
    chunkSize=512
    n=int(math.ceil(len(result)/chunkSize))
    for i in range(n):
      start=chunkSize*i
      end=chunkSize*(i+1)
      print('sending '+str(i))
      packet=ProxybackMessage()
      packet.createProxybackMessage(reqid, i, (i==n-1), result[start:end])
      self.router.sendto(packet.msg, addr, service='httpProxyback')
