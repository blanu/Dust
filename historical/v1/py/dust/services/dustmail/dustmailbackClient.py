from dust.util.jsonrpc.proxy import ServiceProxy

class DustmailbackClient(ServiceProxy):
  def __init__(self, router):
    ServiceProxy.__init__(self, router, serviceName="dustmailback")
