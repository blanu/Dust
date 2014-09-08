package dust.crypto;

import java.lang.Math;
import java.nio.ByteBuffer;

import nl.warper.skein.SkeinMain;
import dust.core.Util;

public class SkeinCipherOFB
{
  static final int INTERNAL_BLOCK_SIZE=256;
  static final int BLOCK_SIZE=32;

  byte[] key;
  byte[] iv;
  byte[] entropy;
  String pers=null;

  static public byte[] encrypt(byte[] key, byte[] iv, byte[] data)
  {
    SkeinCipherOFB cipher=new SkeinCipherOFB(key, iv);
    return cipher.encrypt(data);
  }

  static public byte[] decrypt(byte[] key, byte[] iv, byte[] data)
  {
    SkeinCipherOFB cipher=new SkeinCipherOFB(key, iv);
    return cipher.decrypt(data);
  }

  public SkeinCipherOFB(byte[] k, byte[] i)
  {
    this(k, i, null);
  }

  public SkeinCipherOFB(byte[] k, byte[] i, String p)
  {
    key=k;
    iv=i;
    pers=p;

    entropy="".getBytes();
  }

  public ByteBuffer encrypt(ByteBuffer data)
  {
    byte[] b=encrypt(data.array());
    ByteBuffer result=ByteBuffer.allocate(b.length);
    result.put(b);
    result.flip();
    return result;
  }

  public ByteBuffer decrypt(ByteBuffer data)
  {
    byte[] b=decrypt(data.array());
    ByteBuffer result=ByteBuffer.allocate(b.length);
    result.put(b);
    result.flip();
    return result;
  }

  public byte[] encrypt(byte[] data)
  {
    entropy=getBytes(data.length);
    return Util.xor(data, entropy);
  }

  public byte[] decrypt(byte[] data)
  {
    return encrypt(data);
  }

  private byte[] getBytes(int n)
  {
    byte[] b=new byte[n];

    int offset=0;

    if(b.length<=entropy.length)
    {
      System.arraycopy(entropy, 0, b, 0, b.length);
      int left=entropy.length-b.length;
      byte[] rest=new byte[left];
      System.arraycopy(entropy, b.length, rest, 0, left);
      entropy=rest;
    }
    else
    {
      System.arraycopy(entropy, 0, b, 0, entropy.length);
      offset=entropy.length;

      while(offset<b.length)
      {
        byte[] result=null;
        if(pers!=null)
        {
          byte[] r=SkeinMain.fullSkein(INTERNAL_BLOCK_SIZE, key, pers.getBytes(), null, null, iv, null, BLOCK_SIZE*8);
          System.out.println("skein "+INTERNAL_BLOCK_SIZE+" "+(BLOCK_SIZE*8)+" "+Util.encode(key)+" "+Util.encode(pers.getBytes())+" "+Util.encode(iv)+" -> "+Util.encode(r));
          iv=r;
        }
        else
        {
          byte[] r=SkeinMain.fullSkein(INTERNAL_BLOCK_SIZE, key, null, null, null, iv, null, BLOCK_SIZE*8);
          System.out.println("skein "+INTERNAL_BLOCK_SIZE+" "+(BLOCK_SIZE*8)+" "+Util.encode(key)+" "+Util.encode(iv)+" -> "+Util.encode(r));
          iv=r;
        }

        int bleft=b.length-offset;
        if(bleft<=iv.length)
        {
          System.arraycopy(iv, 0, b, offset, bleft);
          offset=offset+bleft;
          int eleft=iv.length-bleft;
          byte[] rest=new byte[eleft];
          System.arraycopy(iv, bleft, rest, 0, rest.length);
          entropy=rest;
        }
        else
        {
          System.arraycopy(iv, 0, b, offset, iv.length);
          offset=offset+iv.length;
        }
      }
    }

    return b;
  }
}