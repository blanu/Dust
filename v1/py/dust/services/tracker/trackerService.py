from dust.core.util import encodeAddress
from dust.util.jsonrpc.serviceHandler import ServiceHandler
from dust.services.tracker.trackerHandler import TrackerHandler

class TrackerService:
  def __init__(self):
    self.router=None

  def setRouter(self, r):
    self.router=r;

  def handle(self, msock, msg, addr):
    print('Tracker message from '+encodeAddress(addr)+': '+msg.decode('ascii'))
    tracker=ServiceHandler(TrackerHandler(self.router, addr))
    print('tracker: '+str(tracker))
    tracker.handleRequest(msg.decode('ascii'))
