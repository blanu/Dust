from core.util import encodeAddress

class ChatHandler:
  def __init__(self):
    pass

  def handle(self, msg, addr):
    print('Message from '+encodeAddress(addr)+':')
    print(msg.decode('ascii'))
    print('-----------------')
    