from dust.crypto.curve import Keypair, Key
from dust.core.util import encode, decode

keypair=Keypair(Key(decode('48ef7b8c0c6343b332a02862335c11e0cd2b38f0d7d6a3647a7f8f2661d1c946'), False), Key(decode('43aafb64bc96460f3928f6068b2a01aa87bac16da6dc034b4525d1837e9cb85e'), False))
session=keypair.createSessionBytes(decode('77516be967ad1a68632a3c8b08c9e032d23ff5b764a7fe49461b9c12b2da0c32'))
print(encode(session))

