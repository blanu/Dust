import os
import sys
import time
from threading import Thread, Event
from queue import Queue, Empty
from subprocess import Popen, PIPE

from dust.extensions.utp.dustUtpSocket import DustUtpSocket

class Inetd:
  def __init__(self, inq, outq):
    self.inq=inq
    self.outq=outq

  def start(self):
    self.proc=Popen(['fossil', 'http', '/home/blanu/fossil/'], stdin=PIPE, stdout=PIPE)
#    self.proc=Popen(['fossil', 'help'], stdin=PIPE, stdout=PIPE)

    t=Thread(target=self.processIn)
    t.setDaemon(True)
    t.start()

    t2=Thread(target=self.processOut)
#    t2.setDaemon(True)
    t2.start()

  def processIn(self):
    self.proc.poll()
    while not self.proc.returncode:
      data=self.inq.get()
      print('inetd -> '+str(data))
      self.proc.stdin.write(data)
      self.proc.stdin.flush()
      self.proc.poll()

  def processOut(self):
    print('waiting to finish')
#    self.proc.wait()
    print('finished')
    print('waiting to read from inetd')
    data=self.proc.stdout.read()
    print('inetd <- '+str(data))
    self.outq.put(data)

if __name__=='__main__':
  inq=Queue()
  outq=Queue()

  outq.put("GET / HTTP/1.0\n".encode('ascii'))
  outq.put("\n".encode('ascii'))
  inetd=Inetd(outq, inq)

  inetd.start()
