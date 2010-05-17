package dust.crypto;

import dust.crypto.Curve;
import dust.crypto.Keypair;

public class KeyManager
{
  Keypair keypair;
  byte[] entropy;
/*
  public KeyManager()
  {
    self.knownHosts=None
    self.keypair=None
    self.incomingInvites=None
    self.outgoingInvites=None
    self.invitePassword=None
    entropy=new DustPRNG();
  }

  def loadKeypair(self, filename):
    f=open(filename, 'r')
    pair=yaml.load(f.read())
    f.close()
    pubkey=decode(pair[0])
    privkey=decode(pair[1])
    self.keypair=Keypair(Key(privkey, False), Key(pubkey, False))

  public void createKeypair()
  {
    byte[] rb=entropy.getBytes(32);
    keypair=Curve.createKeypair(rb);
  }

  def saveKeypair(self, filename):
    pubkey=encode(self.keypair.public.bytes)
    privkey=encode(self.keypair.secret.bytes)
    pair=[pubkey, privkey]

    f=open(filename, 'w')
    f.write(yaml.dump(pair))
    f.close()

  def getKeypair(self):
    return self.keypair

  def loadKnownHosts(self, filename):
    self.knownHosts={}
    if os.path.exists(filename):
      f=open(filename, 'r')
      hosts=yaml.load(f.read())
      f.close()

      for address, pubkey in hosts.items():
        self.knownHosts[address]=Key(decode(pubkey), False)

  def saveKnownHosts(self, filename):
    hosts={}

    for address, pubkey in self.knownHosts.items():
      hosts[address]=encode(pubkey.bytes)

    f=open(filename, 'w')
    f.write(yaml.dump(hosts))
    f.close()

  def isKnown(self, address):
    return address in self.knownHosts

  def getKeyForHost(self, address):
    return self.knownHosts[address]

  def addHost(self, address, pubkey):
    print('addHost:', address, pubkey)
    addressKey=encodeAddress(address)
    self.knownHosts[addressKey]=pubkey
    print('knownHosts:', self.knownHosts)

  def getSessionKeyForHost(self, address):
    addressKey=encodeAddress(address)
    return self.getSessionKeyForAddress(addressKey)

  def getSessionKeyForAddress(self, addressKey):
    try:
      pubkey=self.knownHosts[addressKey]
    except:
      print('Unknown hosts', addressKey)
      return None
    print('pubkey:', pubkey)
    sessionKey=hash(self.keypair.createSession(pubkey).bytes)
    return sessionKey

  def setInvitePassword(self, passwd):
    self.invitePassword=passwd

  def loadIncomingInvites(self, filename, passwd=None):
    if not passwd:
      passwd=self.invitePassword
    if passwd:
      self.incomingInvites=loadInvitePackage(filename, self.invitePassword)
    else:
      print('No invite password')

  def saveIncomingInvites(self, filename, passwd=None):
    if not passwd:
      passwd=self.invitePassword
    if passwd and self.incomingInvites:
      self.incomingInvites.save(filename, self.invitePassword, self.entropy)
    else:
      print('No invite password or no invites')

  def loadOutgoingInvites(self, filename, passwd=None):
    if not passwd:
      passwd=self.invitePassword
    if passwd:
      self.outgoingInvites=loadInvitePackage(filename, self.invitePassword)
    else:
      print('No invite password')

  def saveOutgoingInvites(self, filename, passwd=None):
    if not passwd:
      passwd=self.invitePassword
    if passwd and self.outgoingInvites:
      self.outgoingInvites.save(filename, self.invitePassword, self.entropy)
    else:
      print('No invite password or no invites')
      */
}