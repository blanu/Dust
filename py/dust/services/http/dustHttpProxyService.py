import bz2
import math
import time
import traceback
import http.client as httplib
from urllib.parse import urlparse
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

    try:
      host=urlparse(url).netloc
      conn=httplib.HTTPConnection(host)
      conn.request('GET', url)
      resp=conn.getresponse()
      if resp.version==10:
        result=b"HTTP/1.0"
      elif resp.version==11:
        result=b"HTTP/1.1"
      else:
        result=b"HTTP/1.1"

      result=result+b' '+bytes(str(resp.status), 'ascii')+b' '+bytes(resp.reason, 'ascii')+b"\r\n"
      for key, value in resp.getheaders():
        result=result+bytes(key, 'ascii')+b': '+bytes(value, 'ascii')+b"\r\n"
      result=result+b"\r\n"
      result=result+resp.read()
    except Exception as e:
      print(e)
      traceback.print_exc()
      result=b"HTTP/1.1 500 Error\r\n\r\n"

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
      time.sleep(1)
