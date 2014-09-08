import os
import sys
import time
from threading import Thread, Event
from queue import Queue, Empty

from dust.extensions.utp.dustUtpSocket import DustUtpSocket

class LineReader:
  def __init__(self, outq):
    self.outq=outq

  def start(self):
    t=Thread(target=self.run)
    t.setDaemon(True)
    t.start()

  def run(self):
    while True:
      try:
        line=sys.stdin.readline()
      except:
        self.outq.put(None)
        return
      self.outq.put(line.encode('ascii'))

class LinePrinter:
  def __init__(self, inq):
    self.inq=inq

  def start(self):
    t=Thread(target=self.run)
    t.setDaemon(True)
    t.start()

  def run(self):
    while True:
      line=self.inq.get()
      sys.stderr.write(line.decode('ascii')+"\n")
      sys.stderr.flush()

if __name__=='__main__':
  if sys.argv[1]=='-s':
    server=True
  else:
    server=False

  inq=Queue()
  outq=Queue()

  lr=LineReader(inq)
  lp=LinePrinter(outq)
  dus=DustUtpSocket(server, inq, outq)

  lr.start()
  lp.start()
  dus.start()
