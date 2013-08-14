package dust.crypto;

import java.nio.ByteBuffer;

import dust.crypto.DustConstants;
import dust.crypto.SkeinUtil;

public class DustUtil
{
  static public byte[] hash(byte[] data)
  {
    return SkeinUtil.hash(data, DustConstants.HASH_PERS);
  }

  static public byte[] mac(byte[] key, ByteBuffer data)
  {
    return DustUtil.mac(key, data.array());
  }

  static public byte[] mac(byte[] key, byte[] data)
  {
    return SkeinUtil.mac(data, key, DustConstants.MAC_PERS);
  }

  static public byte[] pbkdf(byte[] pb, byte[] salt)
  {
    return SkeinUtil.pbkdf(pb, salt, DustConstants.PBKDF_ITERATIONS, DustConstants.PBKDF_PERS);
  }
}