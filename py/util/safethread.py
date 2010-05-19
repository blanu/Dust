import traceback
from threading import Thread, Event

# If you don't do it this way, ctrl-c doesn't work anymore.
def waitForEvent(event):
  while not event.isSet():
    event.wait(1000)

def waitFor(delay):
#  Event().wait(delay)
  import time
  time.sleep(delay)

def wait():
#  Event().wait()
  import time
  while True:
    time.sleep(1000)

class SafeThread(Thread):
  def __init__(self, target=None, args=None):
    self.method=target
    self.args=args
    Thread.__init__(self, target=self.safeCall)

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

