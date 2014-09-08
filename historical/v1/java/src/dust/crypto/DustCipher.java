package dust.crypto;

import java.nio.ByteBuffer;

import dust.crypto.DustConstants;

public class DustCipher extends SkeinCipherOFB
{
  static public byte[] encrypt(byte[] k, byte[] iv, byte[] data)
  {
    DustCipher cipher=new DustCipher(k, iv);
    return cipher.encrypt(data);
  }

  static public byte[] decrypt(byte[] k, byte[] iv, byte[] data)
  {
    DustCipher cipher=new DustCipher(k, iv);
    return cipher.decrypt(data);
  }

  static public ByteBuffer encrypt(byte[] k, byte[] iv, ByteBuffer data)
  {
    DustCipher cipher=new DustCipher(k, iv);
    return cipher.encrypt(data);
  }

  static public ByteBuffer decrypt(byte[] k, byte[] iv, ByteBuffer data)
  {
    DustCipher cipher=new DustCipher(k, iv);
    return cipher.decrypt(data);
  }

  public DustCipher(byte[] key, byte[] iv)
  {
    super(key, iv, DustConstants.CIPHER_PERS);
  }
}
