package dust.test;

import java.util.Random;
import java.util.Arrays;

import dust.core.Util;

public class TestUtil
{
  static public void main(String[] args) throws Exception
  {
    Random random=new Random();

    byte[] key=new byte[32];
    random.nextBytes(key);

    String s=Util.encode(key);
    byte[] b=Util.decode(s);

    System.out.println(Arrays.equals(key, b));

    int i1=7;
    b=Util.intToByteArray(i1, 4);
    int i2=Util.byteArrayToInt(b);
    System.out.println(i1+" "+i2);
  }
}