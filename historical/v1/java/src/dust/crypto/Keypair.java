package dust.crypto;

import dust.crypto.Curve;

public class Keypair
{
  Key secret;
  Key pub;

  static public Keypair create(byte[] entropy)
  {
    return Curve.createKeypair(entropy);
  }

  public Keypair(Key s, Key p)
  {
    secret=s;
    pub=p;
  }

  public Key getSecret()
  {
    return secret;
  }

  public Key getPublic()
  {
    return pub;
  }

  public Key session(Key pub2)
  {
    return new Key(Curve.createShared(secret.getBytes(), pub2.getBytes()));
  }

  public String toString()
  {
    return "<Keypair("+secret+","+pub+")>";
  }
}
