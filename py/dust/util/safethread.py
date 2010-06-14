import sys
import traceback
from threading import Thread, Event

# If you don't do it this way, ctrl-c doesn't work anymore.
def waitForEvent(event):
  done=False
  while not event.isSet() and not done:
    try:
      event.wait(1000)
    except KeyboardInterrupt:
      done=True

def waitFor(delay):
#  Event().wait(delay)
  import time
  time.sleep(delay)

def wait():
#  Event().wait()
  import time
  done=False
  while not done:
    try:
      time.sleep(1000)
    except KeyboardInterrupt:
      print('Ctrl-C')
      done=True

def waitForThreads(threads):
  while len(threads) > 0:
    try:
      print('Threads before: '+str(threads))
      threads = [t.join(1) for t in threads if t is not None and t.isAlive()]
      print('Threads after: '+str(threads))
    except KeyboardInterrupt:
      print("Ctrl-c received! Sending kill to threads...")
      for t in threads:
        t.done = True

class SafeThread(Thread):
  def __init__(self, target=None, args=None):
    Thread.__init__(self, target=self.safeCall)
    self.method=target
    self.args=args
    self.done=False

  def safeCall(self):
    if self.method==None:
      return
    try:
      if self.args==None:
        self.method()
      else:
        self.method(*self.args)
    except Exception:
      print('Internal crash')
      traceback.print_exc()
