from dust.crypto.dustUtil import hash
from dust.core.dust_socket import dust_socket
from dust.core.util import encodeAddress, xor, encode

class lite_socket(dust_socket):
  def makeSession(self, address, tryInvite):
#    addressKey=encodeAddress(address)
    addressKey=address[0]
    print('addressKey: '+str(addressKey))
    if addressKey in self.sessionKeys:
      return self.sessionKeys[addressKey]

    h1=hash(addressKey.encode('ascii'))
    print('h1: '+str(encode(h1)))
    h2=hash(self.myAddressKey.encode('ascii'))
    print('h2: '+str(encode(h2)))

    sessionKey=xor(h1, h2)
    print('Lite Session Key: '+str(encode(sessionKey)))

    self.sessionKeys[addressKey]=sessionKey
    print('SessionKey:', len(self.sessionKeys[addressKey]))
    return sessionKey
