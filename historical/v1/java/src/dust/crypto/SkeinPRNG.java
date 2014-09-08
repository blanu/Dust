package dust.crypto;

import java.util.Random;
import java.nio.ByteBuffer;

import nl.warper.skein.SkeinMain;
import dust.core.Util;

public class SkeinPRNG
{
  static final int SEED_SIZE=16;
  static final int BLOCK_SIZE_BYTES=32;
  static final int BLOCK_SIZE_BITS=BLOCK_SIZE_BYTES*8;

  Random random;
  byte[] seed;
  String pers;

  public SkeinPRNG()
  {
    this(null);
  }

  public SkeinPRNG(String p)
  {
    pers=p;
    random=new Random();
    seed=generateSeed();
  }

  public byte[] generateSeed()
  {
    byte[] rb=new byte[SEED_SIZE];
    random.nextBytes(rb);
    return rb;
  }

  public byte[] getBytes(int n)
  {
    ByteBuffer result=ByteBuffer.allocate(n);
    byte[] b=null;

    while(result.remaining()>0)
    {
      if(pers!=null)
      {
        b=SkeinMain.fullSkein(BLOCK_SIZE_BITS, null, pers.getBytes(), null, null, null, seed, BLOCK_SIZE_BITS);
      }
      else
      {
        b=SkeinMain.fullSkein(BLOCK_SIZE_BITS, null, null, null, null, null, seed, BLOCK_SIZE_BITS);
      }

      System.arraycopy(b, 0, seed, 0, SEED_SIZE);
      result.put(b, SEED_SIZE, Math.min(BLOCK_SIZE_BYTES-SEED_SIZE, result.remaining()));
    }

    return result.array();
  }

  public int getInt(int max)
  {
    byte[] b=getBytes(4);
    int l=Math.abs(Util.byteArrayToInt(b));
    if(max!=0)
    {
      l=l%max;
    }

    return l;
  }
}