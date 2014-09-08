from dust.core.util import encodeAddress
from dust.util.jsonrpc.serviceHandler import ServiceHandler
from dust.services.tracker.trackbackHandler import TrackbackHandler

class TrackbackService:
  def __init__(self):
    self.router=None

  def setRouter(self, r):
    self.router=r;

  def handle(self, msock, msg, addr):
    print('Tracker message from '+encodeAddress(addr)+':')
    trackback=ServiceHandler(TrackbackHandler(self.router))
    trackback.handleRequest(msg.decode('utf-8'))
