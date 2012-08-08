import monocle
from monocle import _o
monocle.init('tornado')

from monocle.core import Callback, Return
from monocle.stack import eventloop

from monocle.stack.network import Connection, ConnectionLost
from monocle.experimental import Channel

buffsize=1024

class FakeSocket(object):
  def __init__(self, a=None, b=None):
    if a==None:
      self.a=Channel(buffsize)
    else:
      self.a=a
    if b==None:
      self.b=Channel(buffsize)
    else:
      self.b=b

    self.inBuff=b''
    self.outBuff=b''

  def invert(self):
    return FakeSocket(a=self.b, b=self.a)

  @_o
  def read(self, x):
    while len(self.inBuff)<x:
      data=yield self.a.recv()
      if data:
        self.inBuff=self.inBuff+data
      else:
        yield ConnectionLost()

    data=self.inBuff[:x]
    self.inBuff=self.inBuff[x:]

    yield Return(data)

  @_o
  def read_some(self):
    if len(self.inBuff)>0:
      data=self.inBuff
      self.inBuff=b''
      yield Return(data)
    else:
      data=b''
      while len(data)==0:
        data=yield self.a.recv()
      if data:
        yield Return(data)
      else:
        yield ConnectionLost()

  @_o
  def write(self, bs):
    yield self.b.send(bs)

  def close(self):
    yield self.b.send(None)
