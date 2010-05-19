from core.util import encodeAddress
from jsonrpc.serviceHandler import ServiceHandler
from dustmail.dustmailHandler import DustmailHandler

class DustmailService:
  def __init__(self):
    self.router=None

  def setRouter(self, r):
    self.router=r;

  def handle(self, msock, msg, addr):
    print('Dustmail message from '+encodeAddress(addr)+': '+msg.decode('ascii'))
    dustmail=ServiceHandler(DustmailHandler(self.router))
    dustmail.handleRequest(msg.decode('ascii'))
