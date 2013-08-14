package dust.crypto;

import dust.crypto.SkeinPRNG;
import dust.crypto.DustConstants;

public class DustPRNG extends SkeinPRNG
{
  public DustPRNG()
  {
    super(DustConstants.PRNG_PERS);
  }
}