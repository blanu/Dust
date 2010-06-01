from dust.util.jsonrpc.proxy import ServiceProxy

class TrackbackClient(ServiceProxy):
  def __init__(self, router, addr):
    ServiceProxy.__init__(self, router, addr=addr, serviceName="trackback")
