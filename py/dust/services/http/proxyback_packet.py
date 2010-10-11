import logging
from dust.core.data_packet import DataPacket
from dust.core.util import splitFields, encode, encodeFlags, decodeFlags

REQID_LENGTH=4
SEQ_LENGTH=1
FLAGS_LENGTH=1

def makeSeqByte(seq):
  b=bytearray(1)
  b[0]=seq
  return bytes(b)

class ProxybackMessage:
  def __init__(self):
    self.reqid=None
    self.seq=None
    self.data=None
    self.msg=None

  def createProxybackMessage(self, reqid, seq, fin, data):
    self.reqid=reqid
    self.seq=seq
    self.fin=fin
    self.data=data

    self.flags=[self.fin, False, False, False, False, False, False, False]

    self.msg=self.reqid+makeSeqByte(seq)+encodeFlags(self.flags)+data

  def decodeProxybackMessage(self, msg):
    self.msg=msg
    self.reqid, self.seq, flagBytes, self.data=splitFields(msg, [REQID_LENGTH, SEQ_LENGTH, FLAGS_LENGTH])
    self.flags=decodeFlags(flagBytes)
    self.fin=self.flags[0]

  def __str__(self):
    s="ProxybackMessage\n"
    s=s+"[\n"
    s=s+"  reqid: "+str(self.reqid)
    s=s+"  seq: "+str(self.seq)
    s=s+"  flags: "+str(self.flags)
    s=s+"  fin: "+str(self.fin)
    s=s+"  data: "+str(self.data)
    s=s+"]\n\n"
    return s
