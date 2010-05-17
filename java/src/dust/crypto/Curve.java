package dust.crypto;

import djb.Curve25519;
import dust.crypto.Keypair;

public class Curve
{
  static public Keypair createKeypair(byte[] entropy)
  {
    byte[] pub=new byte[32];
    byte[] secret=new byte[32];

    System.arraycopy(entropy, 0, secret, 0, 32);

    Curve25519.keygen(pub, null, secret);

    return new Keypair(new Key(secret), new Key(pub));
  }

  static public byte[] createShared(byte[] secret, byte[] pubkey2)
  {
    byte[] shared=new byte[32];

    Curve25519.curve(shared, secret, pubkey2);

    return shared;
  }
}