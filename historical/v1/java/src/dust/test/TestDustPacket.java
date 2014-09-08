package dust.test;

import java.util.Random;

import dust.core.DustPacket;
import dust.core.Util;
import dust.crypto.DustPRNG;

public class TestDustPacket
{
  static public void main(String[] args) throws Exception
  {
    Random random=new Random();

    byte[] key=Util.decode("5475e69147a1463ef65116ccd8b3d732ead5ce8b5c9b0e61eb4c218fe6165013");
//    byte[] key=new byte[32];
//    random.nextBytes(key);
    byte[] data="test #3".getBytes();
    DustPRNG entropy=new DustPRNG();

    DustPacket p1=DustPacket.create(key, data, entropy);
    byte[] bytes=p1.getBytes();

    System.out.println("key: "+Util.encode(key));
    System.out.println("p1: "+Util.encode(bytes));
    System.out.println("mac: "+p1.checkMac()+" timestamp: "+p1.checkTimestamp());

    DustPacket p2=DustPacket.decode(key, bytes);
    System.out.println("p2: "+new String(p2.getData()));
    System.out.println("mac: "+p2.checkMac()+" timestamp: "+p2.checkTimestamp());

    DustPacket p3=DustPacket.decode(key, Util.decode(Util.encode(bytes)));
    System.out.println("p3: "+new String(p3.getData()));
    System.out.println("mac: "+p3.checkMac()+" timestamp: "+p3.checkTimestamp());
  }
}