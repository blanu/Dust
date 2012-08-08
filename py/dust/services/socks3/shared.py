import monocle
from monocle import _o, Return

from monocle.stack.network import ConnectionLost

from dust.core.util import encode

@_o
def pump(input, output, transform, debug=False):
  while True:
    try:
      message = yield input.read_some()
#      message=yield input.read(1)
      if not message or len(message)==0:
        print('0 from '+str(input)+' '+str(type(message)))
        raise(Exception())
#        message=yield input.read(1)
      if debug:
        print('receive '+str(len(message)))
    except ConnectionLost:
      print('Client connection closed')
      output.close()
      break
    except IOError:
      output.close()
      break

    if transform:
      message=yield transform(message)

    if debug:
      print('sending '+str(len(message)))
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
