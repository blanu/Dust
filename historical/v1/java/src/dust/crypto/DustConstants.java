package dust.crypto;

public class DustConstants
{
  static public final int SEED_SIZE=16;
  static public final int IV_SIZE=16;
  static public final int BLOCK_SIZE=32;

//  static public final int PBKDF_ITERATIONS=13000;
  static public final int PBKDF_ITERATIONS=10;

  static public final String MAC_PERS="1978-10-26 dust@blanu.net Dust/MAC";
  static public final String PRNG_PERS="1978-10-26 dust@blanu.net Dust/PRNG";
  static public final String HASH_PERS="1978-10-26 dust@blanu.net Dust/hash";
  static public final String PBKDF_PERS="1978-10-26 dust@blanu.net Dust/PBKDF";
  static public final String CIPHER_PERS="1978-10-26 dust@blanu.net Dust/cipher";
}
