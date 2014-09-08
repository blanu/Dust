package dust.crypto;

import nl.warper.skein.SkeinMain;

public class SkeinUtil
{
  static final int BLOCK_SIZE=256;
  static final int OUTPUT_SIZE=256;

  static public byte[] pbkdf(byte[] pb, byte[] salt, int i)
  {
    return pbkdf(pb, salt, i, null);
  }

  static public byte[] pbkdf(byte[] pb, byte[] salt, int i, String pers)
  {
    int l=pb.length+salt.length;
    byte[] data=new byte[l*i];
    for(int x=0; x<i; x++)
    {
      int index=x*l;
      System.arraycopy(pb, 0, data, index, pb.length);
      System.arraycopy(salt, 0, data, index+pb.length, salt.length);
    }

    if(pers!=null)
    {
      byte[] output = SkeinMain.fullSkein(BLOCK_SIZE, null, pers.getBytes(), null, null, null, data, OUTPUT_SIZE);
      return output;
    }
    else
    {
      byte[] output = SkeinMain.fullSkein(BLOCK_SIZE, null, null, null, null, null, data, OUTPUT_SIZE);
      return output;
    }
  }

  static public byte[] hash(byte[] data)
  {
    return hash(data, null);
  }

  static public byte[] hash(byte[] data, String pers)
  {
    if(pers!=null)
    {
      byte[] output = SkeinMain.fullSkein(BLOCK_SIZE, null, pers.getBytes(), null, null, null, data, OUTPUT_SIZE);
      return output;
    }
    else
    {
      byte[] output = SkeinMain.fullSkein(BLOCK_SIZE, null, null, null, null, null, data, OUTPUT_SIZE);
      return output;
    }
  }

  static public byte[] mac(byte[] key, byte[] data)
  {
    return mac(key, data, null);
  }

  static public byte[] mac(byte[] key, byte[] data, String pers)
  {
    if(pers!=null)
    {
      byte[] output = SkeinMain.fullSkein(BLOCK_SIZE, key, pers.getBytes(), null, null, null, data, OUTPUT_SIZE);
      return output;
    }
    else
    {
      byte[] output = SkeinMain.fullSkein(BLOCK_SIZE, key, null, null, null, null, data, OUTPUT_SIZE);
      return output;
    }
  }
}