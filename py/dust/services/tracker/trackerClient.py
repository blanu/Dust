from dust.util.jsonrpc.proxy import ServiceProxy

class TrackerClient(ServiceProxy):
  def __init__(self, router):
    ServiceProxy.__init__(self, router, serviceName="tracker")
