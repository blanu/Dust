package dust.core;

import java.nio.ByteBuffer;

public class Util
{
  static public String encode(byte[] b)
  {
    try{
    StringBuffer buff=new StringBuffer();
    for(int x=0;x<b.length;x++)
    {
      int i=b[x];
      if(i<0) i=i+256;
      if(i<16) buff.append("0");
      buff.append(Integer.toString(i, 16));
    }
    return buff.toString();
    }
    catch(Exception e)
    {
      return null;
    }
  }

  public static String encode(final ByteBuffer buffer)
  {
    byte[] bytes=buffer.array();
    return encode(bytes);
  }

  public static byte[] decode(String in) {
    int len = in.length();
    if (len % 2 != 0) {
      throw new IllegalArgumentException("Even length string expected.");
    }
    byte[] out = new byte[len/2];
    try {
      for (int i = 0; i < out.length; i++) {
        out[i] = (byte)(Integer.parseInt(in.substring(i*2, i*2+2), 16));
      }
    } catch (NumberFormatException doh) {
    doh.printStackTrace();
    throw new IllegalArgumentException("ParseError");
    }
    return out;
  }

  /*
def encodeAddress(addr):
  ip=addr[0]
  if ip=='':
    ip='::'
  port=addr[1]
  if '.' in ip:
    return ip+':'+str(port)
  else:
    return '['+ip+']:'+str(port)

def decodeAddress(s):
  if '.' in s:
    parts=s.split(':')
    return (parts[0], int(parts[1]), False)
  else:
    m=re.match('\[([0-9a-f:]+)\]:([0-9]+)', s)
    return (m.group(1), int(m.group(2)), True)

def getPublicIP(v6=True):
  if v6:
    text=urlopen("http://ipv6.ip6.me/").read()
    match=re.search(b"\+3>([^<]+)<", text)
#    ip=urlopen("http://whatismyv6ip.com/myip").read()
#    return ip.decode('ascii')
    ip=match.group(1)
    return ip.decode('ascii')
  else:
    text=urlopen("http://ip4.me/").read()
    match=re.search(b"\+3>([^<]+)<", text)
#    ip=urlopen("http://whatismyv6ip.com/myip").read()
#    return ip.decode('ascii')
    ip=match.group(1)
    return ip.decode('ascii')

def getAddress(port):
  return encodeAddress((getPublicIP(), port))
*/

  static public void splitFields(byte[] msg, byte[] first, byte[] second)
  {
    System.arraycopy(msg, 0, first, 0, first.length);
    System.arraycopy(msg, first.length+1, second, 0, second.length);
  }

  static public void splitField(byte[] msg, byte[] field)
  {
    System.arraycopy(msg, 0, field, 0, field.length);
  }

  /*
def decodeFlags(flagsByte):
  bits=BitString(bytes=flagsByte)
  bools=[]
  for x in range(bits.length):
    bools.append(bits.readbit().uint==1)
  return bools

def encodeFlags(bools):
  bits=BitString()
  for bool in bools:
    if bool:
      bits.append(BitString('0b1'))
    else:
      bits.append(BitString('0b0'))
  return bits.bytes

def fill(bytes, size):
  while len(bytes)<size:
    bytes=bytes+b'\x00'
  return bytes
*/

  static public byte[] xor(byte[] a, byte[] b)
  {
    System.out.println("xor "+encode(a)+" "+encode(b));
    if(a.length!=b.length)
    {
      System.out.println("xor parameters must be the same length: "+a.length+" "+b.length);
      return null;
    }
    else
    {
      byte[] c=new byte[a.length];
      for(int x=0; x<c.length; x++)
      {
        c[x]=(byte)(a[x] ^ b[x]);
      }
      return c;
    }
  }

  public static byte[] longToByteArray(long value)
  {
    return new byte[] {(byte)(value >>> 24), (byte)(value >>> 16), (byte)(value >>> 8), (byte)value};
  }

  public static long byteArrayToLong(byte[] b)
  {
    return (b[0] << 24) + ((b[1] & 0xFF) << 16) + ((b[2] & 0xFF) << 8) + (b[3] & 0xFF);
  }

  public static byte[] intToByteArray(int value, int size)
  {
    byte[] b=new byte[size];
    for(int x=0; x<b.length; x++)
    {
      int shiftBy=(b.length-(x+1))*8;
      b[x]=(byte)(value >>> shiftBy);
    }
    return b;
  }

  public static int byteArrayToInt(byte[] b)
  {
    int result=0;
    for(int x=0; x<b.length; x++)
    {
      int shiftBy=(b.length-(x+1))*8;
      result=result+((b[x]&0xFF)<<shiftBy);
    }
    return result;
  }
}