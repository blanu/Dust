package dust.core;

import dust.core.DustPacket;
import dust.crypto.DustPRNG;

public class DataPacket extends DustPacket
{
  static public DataPacket create(byte[] key, byte[] data, DustPRNG entropy)
  {
    return new DataPacket(key, data, entropy);
  }

  static public DataPacket decode(byte[] key, byte[] packet)
  {
    return new DataPacket(key, packet);
  }

  public DataPacket(byte[] key, byte[] data)
  {
    super(key, data);
  }

  public DataPacket(byte[] key, byte[] data, DustPRNG entropy)
  {
    super(key, data, entropy);
  }
}





















