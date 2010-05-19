package dust.test;

import java.util.Random;

import dust.crypto.SkeinUtil;
import dust.crypto.SkeinPRNG;
import dust.crypto.SkeinCipherOFB;

import dust.core.Util;

public class TestSkein
{
  static public void main(String[] args) throws Exception
  {
    byte[] entropy=new byte[32];
    Random random=new Random();
    random.nextBytes(entropy);

    /*
    byte[] pass="test".getBytes();
    int iterations=10;
    byte[] pbkdf=SkeinUtil.pbkdf(pass, entropy, iterations);

    System.out.println("pbkdf: "+Util.encode(pbkdf));
    */

    byte[] data="test data".getBytes();
    byte[] hash=SkeinUtil.hash(data);
    System.out.println("hash: "+Util.encode(hash));
    hash=SkeinUtil.hash(data, "a");
    System.out.println("hash pers: "+Util.encode(hash));

    /*
    SkeinPRNG prng=new SkeinPRNG();
    byte[] rb=prng.getBytes(1);
    System.out.println("prng: "+Util.encode(rb));
    rb=prng.getBytes(1);
    System.out.println("prng: "+Util.encode(rb));
    rb=prng.getBytes(10);
    System.out.println("prng: "+Util.encode(rb));
    rb=prng.getBytes(32);
    System.out.println("prng: "+Util.encode(rb));
    */

    /*
    byte[] key=new byte[32];
    random.nextBytes(key);
    byte[] iv=new byte[32];
    random.nextBytes(iv);
    SkeinCipherOFB cipher=new SkeinCipherOFB(key, iv);
    byte[] enc=cipher.encrypt(data);
    byte[] dec=cipher.decrypt(enc);
    System.out.println("cipher: "+new String(data)+" "+new String(enc)+" "+new String(dec));
    */

    /*
    byte[] key=Util.decode("5475e69147a1463ef65116ccd8b3d732ead5ce8b5c9b0e61eb4c218fe6165013");
    byte[] iv=Util.decode("5475e69147a1463ef65116ccd8b3d732ead5ce8b5c9b0e61eb4c218fe6165013");
    byte[] enc=SkeinCipherOFB.encrypt(key, iv, data);
    byte[] dec=SkeinCipherOFB.decrypt(key, iv, enc);
    System.out.println("cipher: "+new String(data)+" "+Util.encode(enc)+" "+new String(dec));
    */
  }
}