from util.jsonrpc.proxy import ServiceProxy

class DustmailClient(ServiceProxy):
  def __init__(self, router):
    ServiceProxy.__init__(self, router, serviceName="dustmail")
