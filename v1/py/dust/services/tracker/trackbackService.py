from dust.core.util import encodeAddress
from dust.util.jsonrpc.serviceHandler import ServiceHandler
from dust.services.tracker.trackbackHandler import TrackbackHandler

class TrackbackService:
  def __init__(self):
    self.router=None

  def setRouter(self, r):
    self.router=r;
    self.handler=TrackbackHandler(self.router.keys)
    self.trackback=ServiceHandler(self.handler)

  def setPutPeerForEndpointCallback(self, key, callback):
    self.handler.setPutPeerForEndpointCallback(key, callback)

  def setPutInviteForPeerCallback(self, key, callback):
    self.handler.setPutInviteForPeer(key, callback)

  def setPutTrackerInviteCallback(self, callback):
    self.handler.setPutTrackerInvite(callback)

  def handle(self, msock, msg, addr):
    print('Trackback message from '+encodeAddress(addr)+':')
    self.trackback.handleRequest(msg.decode('utf-8'))
