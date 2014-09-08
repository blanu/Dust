from dust.crypto.dustUtil import hash
from dust.core.dust_socket import dust_socket
from dust.core.util import encodeAddress, xor, encode

class lite_socket(dust_socket):
  def makeSession(self, address, tryInvite):
#    addressKey=encodeAddress(address)
    addressKey=address[0]
    if addressKey in self.sessionKeys:
      return self.sessionKeys[addressKey]

    h1=hash(addressKey.encode('ascii'))
    h2=hash(self.myAddress[0].encode('ascii'))

    sessionKey=xor(h1, h2)

    self.sessionKeys[addressKey]=sessionKey
    print('SessionKey:', len(self.sessionKeys[addressKey]))
    return sessionKey
