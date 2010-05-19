package dust.test;

import java.util.Random;

import dust.crypto.DustUtil;
import dust.crypto.DustPRNG;
import dust.crypto.DustCipher;
import dust.core.Util;

public class TestDustCrypto
{
  static public void main(String[] args) throws Exception
  {
    byte[] entropy=new byte[32];
    Random random=new Random();
    random.nextBytes(entropy);

    byte[] pass="test".getBytes();
    byte[] pbkdf=DustUtil.pbkdf(pass, entropy);

    System.out.println("pbkdf: "+Util.encode(pbkdf));

    byte[] data="test data".getBytes();
    byte[] hash=DustUtil.hash(data);
    System.out.println("hash: "+Util.encode(hash));

    DustPRNG prng=new DustPRNG();
    byte[] rb=prng.getBytes(1);
    System.out.println("prng: "+Util.encode(rb));
    rb=prng.getBytes(1);
    System.out.println("prng: "+Util.encode(rb));
    rb=prng.getBytes(10);
    System.out.println("prng: "+Util.encode(rb));
    rb=prng.getBytes(32);
    System.out.println("prng: "+Util.encode(rb));

//    byte[] key=new byte[32];
//    random.nextBytes(key);
//    byte[] iv=new byte[32];
//    random.nextBytes(iv);
    byte[] key=Util.decode("5475e69147a1463ef65116ccd8b3d732ead5ce8b5c9b0e61eb4c218fe6165013");
    byte[] iv=Util.decode("5475e69147a1463ef65116ccd8b3d732ead5ce8b5c9b0e61eb4c218fe6165013");
    byte[] enc=DustCipher.encrypt(key, iv, data);
    byte[] dec=DustCipher.decrypt(key, iv, enc);
    System.out.println("cipher: "+new String(data)+" "+Util.encode(enc)+" "+new String(dec));
  }
}