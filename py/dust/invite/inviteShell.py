from dust.crypto.keys import KeyManager

class InviteShell:
  def __init__(self, keys):
    self.keys=keys

  def run(self):
    self.printHelp()
    command=self.getCommand()
    while command!='q':
      self.executeCommand(command)
      command=self.getCommand()

  def printHelp(self):
    print('h: help')
    print('a: add invite')
    print('d: delete invite')
    print('l: list invites')
    print('li: list incoming invites')
    print('lo: list outgoing invites')

  def getCommand(self):
    return input('> ').strip()

  def executeCommand(self, command):
    if command=='h':
      self.printHelp()
    elif command=='a':
      pass
    elif command=='d':
      pass
    elif command=='l':
      print('Incoming:')
      print(keys.incomingInvites)
      print()
      print('Outgoing:')
      print(keys.outgoingInvites)
    elif command=='li':
      print('Incoming:')
      print(keys.incomingInvites)
    elif command=='lo':
      print('Outgoing:')
      print(keys.outgoingInvites)

if __name__=='__main__':
  passwd=input('Password: ')
  keys=KeyManager()
  keys.setInvitePassword(passwd)
  keys.loadKnownHosts('config/knownhosts.yaml')
  keys.loadKeypair('config/id.yaml')
  keys.loadIncomingInvites('config/incoming_invites.ip')
  keys.loadOutgoingInvites('config/outgoing_invites.ip')

  shell=InviteShell(keys)
  shell.run()
