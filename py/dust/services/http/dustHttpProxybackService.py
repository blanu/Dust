import bz2
import urllib.request
from dust.core.util import encode, encodeAddress
from dust.services.http.proxyback_packet import ProxybackMessage

class DustHttpProxybackService:
  def __init__(self):
    self.router=None
    self.qs={}
    self.stats={}

  def setRouter(self, r):
    self.router=r;

  def setQueue(self, reqid, q):
    self.qs[encode(reqid)]=q
    self.stats[encode(reqid)]=[None, None, {}]

  def handle(self, msock, msg, addr):
#    data=msg.decode('ascii')
#    data=bz2.decompress(msg).decode('ascii')
#    print('HTTP Proxyback message from '+encodeAddress(addr)+': ')
#    print(msg)

    packet=ProxybackMessage()
    packet.decodeProxybackMessage(msg)
#    print(packet)

    print('reqid: '+encode(packet.reqid))
    print('current: '+str(packet.seq))

    if not encode(packet.reqid) in self.stats:
      print('Unknown reqid '+encode(packet.reqid))
      print('Dropping packet with seq '+str(packet.seq))
      return

    q=self.qs[encode(packet.reqid)]
    progress, last, chunks=self.stats[encode(packet.reqid)]
    if not packet.seq in chunks: # Store new chunks by seq number
      chunks[packet.seq]=packet.data
    if progress==None and packet.seq==0: # First packet arrived
      progress=0
    if packet.fin: # If fin flag set, this is the last packet in the sequence
      last=packet.seq
    done=False

    print('reqid: '+encode(packet.reqid))
    print('progress: '+str(progress))
    print('last: '+str(last))
    print('current: '+str(packet.seq))

    if progress!=None:
      if last!=None:
        done=True
        for x in range(progress, last+1): # Send all sequential packets from last sent to new packet
          try:
            chunk=chunks[x]
            print('putting chunk '+encode(packet.reqid)+':'+str(x)+'/'+str(last))
            q.put(chunk)
            progress=x+1
          except:
            done=False
            break
      else:
        for x in range(progress, packet.seq+1): # Send all sequential packets from last sent to new packet
          try:
            chunk=chunks[x]
            print('putting chunk '+encode(packet.reqid)+':'+str(x)+'/?')
            q.put(chunk)
            progress=x+1
          except:
            break

    if done:
      q.put(None)
      del self.qs[encode(packet.reqid)]
      del self.stats[encode(packet.reqid)]
    else:
      self.stats[encode(packet.reqid)]=[progress, last, chunks]
    print(q.qsize())
