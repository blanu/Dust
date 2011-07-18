import monocle
from monocle import _o

from monocle.stack.network import ConnectionLost

from dust.core.util import encode
from dust.extensions.lite.lite_socket import lite_socket

@_o
def pump(input, output, transform):
  while True:
    try:
      message = yield input.read_some()
      print('receive '+str(len(message)))
    except ConnectionLost:
      print('Client connection closed')
      output.close()
      break
    except IOError:
      output.close()
      break

    if transform:
      messages=transform(message)
    else:
      messages=[message]

    for x in range(len(messages)):
      message=messages[x]
      print('sending '+str(len(message))+' - '+str(x+1)+'/'+str(len(messages)))
      try:
        yield output.write(message)
      except ConnectionLost:
        print('Connection lost')
        input.close()
        return
      except IOError:
        print('IOError')
        input.close()
        return
      except Exception, e:
        print('Exception')
        print(e)
        input.close()
        return

class DustCoder(object):
  def __init__(self, myAddr, dest):
    self.duster=lite_socket(KeyManager())
    self.duster.setAddress(myAddr)
    self.dest=dest
    self.inbuffer=b''

  def dustPacket(self, message):
    if len(message)<1024:
      messages=[message]
    else:
      messages=[]
      for x in range(len(message)/1024):
        messages.append(message[:1024])
        message=message[1024:]
      if len(message)>0:
        messages.append(message)
    results=[]
    for data in messages:
      result=self.duster.encodePacket(self.dest, data).packet
#      print('Sending packet '+str(len(result))+' - '+str(len(data)))
      results.append(result)
    return results

  def dirtyPacket(self, data):
    self.inbuffer=self.inbuffer+data
    packet=self.duster.decodePacket(self.dest, data)
    if packet: # FIXME -- and packet.checkTimestamp()
      result=packet.data
      if packet.remaining:
        self.inbuffer=packet.remaining
#        print('Good packet: '+str(len(data)-len(packet.remaining))+' - '+str(len(result)))
      else:
        self.inbuffer=b''
#        print('Good packet: '+str(len(data))+' - '+str(len(result)))
      return [result]
    else:
      print('Partial or corrupted packet: '+str(len(data)))
      return []
