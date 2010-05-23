from core.util import encodeAddress
from util.jsonrpc.serviceHandler import ServiceHandler
from tracker.trackbackHandler import TrackbackHandler

class TrackbackService:
  def __init__(self):
    self.router=None
    self.handler=TrackbackHandler()
    self.trackback=ServiceHandler(self.handler)

  def setRouter(self, r):
    self.router=r;

  def setPutPeerForEndpointCallback(self, key, callback):
    self.handler.setPutPeerForEndpointCallback(key, callback)

  def handle(self, msock, msg, addr):
    print('Trackback message from '+encodeAddress(addr)+':')
    self.trackback.handleRequest(msg.decode('utf-8'))
