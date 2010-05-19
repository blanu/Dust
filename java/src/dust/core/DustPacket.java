package dust.core;

import java.util.Date;
import java.util.Arrays;
import java.nio.ByteBuffer;
import java.util.logging.Logger;

import dust.crypto.Curve;
import dust.crypto.DustUtil;
import dust.crypto.DustCipher;
import dust.crypto.DustPRNG;
import dust.core.Util;

public class DustPacket
{
  static Logger log=Logger.getLogger(DustPacket.class.getName());

  static final int PADDING_RANGE = 32;

  static final int IV_SIZE = 16;
  static final int KEY_SIZE = 32;
  static final int MAC_SIZE = 32;
  static final int TIMESTAMP_SIZE=4;
  static final int DATA_LENGTH_SIZE=2;
  static final int PADDING_LENGTH_SIZE=1;

  static final int HEADER_SIZE=IV_SIZE+MAC_SIZE+TIMESTAMP_SIZE+DATA_LENGTH_SIZE+PADDING_LENGTH_SIZE;

  private boolean goodMac=false;

  byte[] key=null;
  byte[] mac=null;
  byte[] iv=null;
  ByteBuffer encrypted=null;

  long timestamp=0;
  byte[] dataLength=null;
  byte[] paddingLength=null;
  byte[] data=null;

  byte[] padding=null;

  ByteBuffer remaining=null;

  ByteBuffer packet=null;

  static public DustPacket create(byte[] key, byte[] data, DustPRNG entropy)
  {
    return new DustPacket(key, data, entropy);
  }

  static public DustPacket decode(byte[] key, byte[] packet)
  {
    return new DustPacket(key, packet);
  }

  public DustPacket(byte[] k, byte[] d, DustPRNG entropy)
  {
    key=k;
    data=d;

    padding=makePadding(entropy, PADDING_RANGE);
    int payloadLength=TIMESTAMP_SIZE+DATA_LENGTH_SIZE+PADDING_LENGTH_SIZE+data.length;
    int packetLength=MAC_SIZE+IV_SIZE+payloadLength+padding.length;

    timestamp=getTime();

    ByteBuffer payload=ByteBuffer.allocate(payloadLength);
    payload.put(Util.longToByteArray(timestamp));
    payload.put(Util.intToByteArray(data.length, DATA_LENGTH_SIZE));
    payload.put(Util.intToByteArray(padding.length, PADDING_LENGTH_SIZE));
    payload.put(data);
    payload.flip();

    iv=makeIV(entropy);
    encrypted=DustCipher.encrypt(key, iv, payload);

    ByteBuffer ciphertext=ByteBuffer.allocate(IV_SIZE+encrypted.remaining());
    ciphertext.put(iv);
    ciphertext.put(encrypted);
    ciphertext.flip();

    mac=makeMac(key, ciphertext);
    goodMac=true;

    packet=ByteBuffer.allocate(packetLength);
    packet.put(mac);
    packet.put(ciphertext);
    packet.put(padding);
    packet.rewind();
  }

  public DustPacket(byte[] k, byte[] p)
  {
    key=k;
    packet=ByteBuffer.allocate(p.length);
    packet.put(p);
    packet.flip();

    log.warning("key: "+Util.encode(key));

    mac=new byte[MAC_SIZE];
    packet.get(mac, 0, MAC_SIZE);

    log.warning("mac: "+Util.encode(mac));

    ByteBuffer ciphertext=packet.slice();

    iv=new byte[IV_SIZE];
    ciphertext.get(iv, 0, IV_SIZE);
    byte[] encrypted=new byte[ciphertext.remaining()];
    ciphertext.get(encrypted);

    log.warning("iv: "+Util.encode(iv));

    byte[] decrypted=DustCipher.decrypt(key, iv, encrypted);
    ByteBuffer payload=ByteBuffer.allocate(decrypted.length);
    payload.put(decrypted);
    payload.flip();

    log.warning("decrypted: "+Util.encode(decrypted));

    byte[] timeBytes=new byte[TIMESTAMP_SIZE];
    byte[] dataLengthBytes=new byte[DATA_LENGTH_SIZE];
    byte[] paddingLengthBytes=new byte[PADDING_LENGTH_SIZE];
    payload.get(timeBytes, 0, TIMESTAMP_SIZE);
    payload.get(dataLengthBytes, 0, DATA_LENGTH_SIZE);
    payload.get(paddingLengthBytes, 0, PADDING_LENGTH_SIZE);

    timestamp=Util.byteArrayToLong(timeBytes);
    int dataLength=Util.byteArrayToInt(dataLengthBytes);
    int paddingLength=Util.byteArrayToInt(paddingLengthBytes);

    log.warning("timestamp: "+timestamp);
    log.warning("dataLength: "+dataLength);
    log.warning("paddingLength: "+paddingLength);

    data=new byte[dataLength];
    payload.get(data, 0, dataLength);

    int payloadLength=TIMESTAMP_SIZE+DATA_LENGTH_SIZE+PADDING_LENGTH_SIZE+data.length;
    payload.limit(payloadLength);

    int ciphertextLength=IV_SIZE+payloadLength;
    ciphertext.limit(ciphertextLength);
    ciphertext.flip();

    byte[] ciphertextBytes=new byte[ciphertext.remaining()];
    ciphertext.get(ciphertextBytes);
    goodMac=Arrays.equals(mac, makeMac(key, ciphertextBytes));

    int realPacketLength=MAC_SIZE+ciphertextLength+paddingLength;
    packet.position(realPacketLength);
    if(packet.remaining()>0)
    {
      remaining=packet.slice();
    }
    else
    {
      remaining=null;
    }
  }

  public byte[] getBytes()
  {
    return packet.array();
  }

  public byte[] getData()
  {
    return data;
  }

  public String toString()
  {
    StringBuffer buff=new StringBuffer();
    buff.append("[\n");

    if(key!=null)
    {
      buff.append("  key: "+Util.encode(key)+"\n");
    }
    else
    {
      buff.append("  key: null\n");
    }

    if(checkMac())
    {
      buff.append("  MAC: "+Util.encode(mac)+" OK\n");
    }
    else
    {
      buff.append("  MAC: "+Util.encode(mac)+" Failed\n");
    }

    buff.append("  IV: "+Util.encode(iv)+"\n");

    if(checkTimestamp())
    {
      buff.append("  timestamp: "+timestamp+" OK\n");
    }
    else
    {
      buff.append("  timestamp: "+timestamp+" Failed\n");
    }

    buff.append("  dataLength: "+dataLength+"\n");
    buff.append("  paddingLength: "+paddingLength+"\n");
    buff.append("  data: "+data+"\n");
    if(padding!=null)
    {
      buff.append("  padding: "+Util.encode(padding)+"\n");
    }
    else
    {
      buff.append("  padding: null\n");
    }
    if(remaining!=null)
    {
      buff.append("  remaining: "+Util.encode(remaining)+"\n");
    }
    else
    {
      buff.append("  remaining: null\n");
    }
    buff.append("]\n");
    return buff.toString();
  }

  public boolean checkMac()
  {
    return goodMac;
  }

  public boolean checkTimestamp()
  {
    long now=getTime();
    long delta=now-timestamp;
    return delta<10;
  }

  // 4 bytes
  private long getTime()
  {
    return new Date().getTime()/1000;
  }

  // 32 bytes
  public byte[] makeMac(byte[] k, ByteBuffer data)
  {
    return DustUtil.mac(k, data);
  }

  public byte[] makeMac(byte[] k, byte[] data)
  {
    return DustUtil.mac(k, data);
  }

  // 16 bytes
  private byte[] makeIV(DustPRNG entropy)
  {
    return entropy.getBytes(IV_SIZE);
  }

  // Random number of bytes 0-size
  private byte[] makePadding(DustPRNG entropy, int range)
  {
    int num=entropy.getInt(range-1);
    return entropy.getBytes(num);
  }
}