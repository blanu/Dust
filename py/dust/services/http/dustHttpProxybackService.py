import bz2
import urllib.request
from dust.core.util import encodeAddress
from dust.services.http.proxyback_packet import ProxybackMessage

class DustHttpProxybackService:
  def __init__(self):
    self.router=None
    self.qs={}

  def setRouter(self, r):
    self.router=r;

  def setQueue(self, reqid, q):
    self.qs[reqid]=q

  def handle(self, msock, msg, addr):
#    data=msg.decode('ascii')
#    data=bz2.decompress(msg).decode('ascii')
    print('HTTP Proxyback message from '+encodeAddress(addr)+': ')
    print(msg)

    packet=ProxybackMessage()
    packet.decodeProxybackMessage(msg)
    print(packet)
    q=self.qs[packet.reqid]
    q.put(packet.data)
    if packet.fin:
      print('EOF')
      q.put(None)
      del self.qs[packet.reqid]
    print(q.qsize())
