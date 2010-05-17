package dust.crypto;

import nl.warper.skein.Util;

public class Key
{
  byte[] bytes;

  public Key(byte[] b)
  {
    bytes=b;
  }

  public byte[] getBytes()
  {
    return bytes;
  }

  public String toString()
  {
    return Util.tohex(bytes);
  }
}