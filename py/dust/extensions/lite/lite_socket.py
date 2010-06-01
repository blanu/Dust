from crypto.dust import hash
from core.dust_socket import dust_socket
from core.util import encodeAddress, xor

class lite_socket(dust_socket):
  def makeSession(self, address, tryInvite):
    addressKey=encodeAddress(address)
    if addressKey in self.sessionKeys:
      return self.sessionKeys[addressKey]

    h1=hash(addressKey.encode('ascii'))
    h2=hash(self.myAddressKey.encode('ascii'))

    sessionKey=xor(h1, h2)

    self.sessionKeys[addressKey]=sessionKey
    print('SessionKey:', len(self.sessionKeys[addressKey]))
    return sessionKey
