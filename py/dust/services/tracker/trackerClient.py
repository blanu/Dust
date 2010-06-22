from dust.util.jsonrpc.proxy import ServiceProxy

class TrackerClient(ServiceProxy):
  def __init__(self, router, addr=None):
    ServiceProxy.__init__(self, router, addr=addr, serviceName="tracker")
