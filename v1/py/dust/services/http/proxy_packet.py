from dust.core.util import splitFields, encode

REQID_LENGTH=4

class ProxyMessage:
  def __init__(self):
    self.reqid=None
    self.data=None
    self.msg=None

  def createProxyMessage(self, reqid, data):
    self.reqid=reqid
    self.data=data

    self.msg=reqid+data

  def decodeProxyMessage(self, msg):
    self.msg=msg
    self.reqid, self.data=splitFields(msg, [REQID_LENGTH])

  def __str__(self):
    s="ProxyMessage\n"
    s=s+"[\n"
    s=s+"  reqid: "+str(self.reqid)
    s=s+"  data: "+str(self.data)
    s=s+"]\n\n"
    return s

