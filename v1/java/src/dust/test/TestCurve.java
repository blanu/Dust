package dust.test;

import java.util.Random;

import dust.crypto.Curve;
import dust.crypto.Keypair;
import dust.crypto.Key;
import dust.core.Util;

public class TestCurve
{
  static public void main(String[] args) throws Exception
  {
    byte[] entropy=new byte[32];
    Random random=new Random();

    random.nextBytes(entropy);
    Keypair pair1=Curve.createKeypair(entropy);
    System.out.println("pair1: "+pair1);

    random.nextBytes(entropy);
    Keypair pair2=Curve.createKeypair(entropy);
    System.out.println("pair2: "+pair2);

    Key session1=pair1.session(pair2.getPublic());
    System.out.println("session1: "+session1);

    Key session2=pair2.session(pair1.getPublic());
    System.out.println("session1: "+session2);
  }
}