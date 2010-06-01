from dust.core.util import encodeAddress

class ChatHandler:
  def __init__(self):
    pass

  def handle(self, msock, msg, addr):
    print('Message from '+encodeAddress(addr)+':')
    print(msg.decode('ascii'))
    print('-----------------')
