import os
import sys
import time
from threading import Thread, Event
from queue import Queue, Empty
from subprocess import Popen, PIPE

from dust.extensions.utp.dustUtpSocketServer import DustUtpSocketServer

class InetdServer:
  def __init__(self):
    server=DustUtpSocketServer()
    server.start()
    waiting=True
    while waiting:
      print('Accepting')
      try:
        inq=Queue()
        outq=Queue()
        dus=server.accept(inq, outq)
        inetd=Inetd(outq, inq)
        inetd.start()
        dus.start()
      except Exception as e:
        print('Exception in accept, exiting: '+str(e))
        waiting=False
        return

class Inetd:
  def __init__(self, inq, outq):
    self.inq=inq
    self.outq=outq

  def start(self):
    self.proc=Popen(['fossil', 'http', '/home/blanu/fossil/'], stdin=PIPE, stdout=PIPE)

    t=Thread(target=self.processIn)
    t.setDaemon(True)
    t.start()

    t2=Thread(target=self.processOut)
    t2.setDaemon(True)
    t2.start()

  def processIn(self):
    self.proc.poll()
    while not self.proc.returncode:
      print('pin')
      data=self.inq.get()
      print('inetd -> '+str(data))
      self.proc.stdin.write(data)
      self.proc.stdin.flush()
      self.proc.poll()

  def processOut(self):
#    self.proc.wait()
    print('waiting to read from inetd')
    data=self.proc.stdout.read(1024)
    while data:
      print('pout')
      print('inetd <- '+str(data))
      self.outq.put(data)
      data=self.proc.stdout.read(1024)
    self.outq.put(None)

if __name__=='__main__':
  server=InetdServer()
  waiting=True
  while waiting:
    try:
      print('Sleeping')
      time.sleep(1)
    except:
      print('Exception waiting')
      waiting=False
      sys.exit(0)
