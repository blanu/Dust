package nl.warper.skein;




/**
 * Some utilities and constants that may be usefull.
 * @author maartenb
 * @author $Author: $
 * @since 5 nov 2008
 * @version $Revision: $
 */
public final class Util {
	
	private Util() {
		// make sure we cannot instantiate the Util class containing static methods
	}
	
	public static final int BYTES_IN_LONG = Long.SIZE / Byte.SIZE; // otherwise known as 8
	private static final int UNSIGNED_BYTE_MASK = 0xFF;

	public static long lsbBytesToLong(final byte[] ba) {
		if (ba == null || ba.length != BYTES_IN_LONG) {
			throw new IllegalArgumentException("Whoops");
		}
		
		long l = 0L;
		for (int i = 0; i < BYTES_IN_LONG; i++) {
			l |= ((long) (ba[i] & 0xFF)) << (i * Byte.SIZE);
		}
		return l;
	}

	public static byte[] lsbLongToBytes(final long l) {
		final byte[] ba = new byte[BYTES_IN_LONG];
		for (int i = 0; i < ba.length; i++) {
			ba[i] = (byte) (l >>> (i  * Byte.SIZE));
		}
		return ba;
	}
	
	public static long[] lsbBytesToArrayOfLong(final byte[] ba) {
		if (ba == null || ba.length % BYTES_IN_LONG != 0) {
			throw new IllegalArgumentException("Whoops");
		}
		
		final int arraySize = ba.length / BYTES_IN_LONG;
		final long[] la = new long[arraySize];
		final byte[] subArray = new byte[BYTES_IN_LONG];
		for (int i = 0; i < arraySize; i++) {
			System.arraycopy(ba, i * BYTES_IN_LONG, subArray, 0, BYTES_IN_LONG);
			la[i] = lsbBytesToLong(subArray);
		}
		return la;
	}
	
	public static byte[] lsbArrayOfLongToBytes(final long[] la) {
		if (la == null) {
			throw new IllegalArgumentException("Whoops");
		}
		final byte[] ba = new byte[la.length * BYTES_IN_LONG];
		byte[] subArray;
		for (int i = 0; i < la.length; i++) {
			subArray = lsbLongToBytes(la[i]);
			System.arraycopy(subArray, 0, ba, i * BYTES_IN_LONG, BYTES_IN_LONG);
		}
		return ba;
	}
	
	public static byte[] zeroPad(final byte[] data, final int blockSize) {
		if (data == null) {
			throw new IllegalArgumentException("Please provide some data to pad");
		}
		
		if (blockSize <= 0 || blockSize % Byte.SIZE != 0) {
			throw new IllegalArgumentException("Blocksize must be a possitive integer N where N % 8 = 0");
		}
		
		final int blockSizeBytes = blockSize / Byte.SIZE;
		
		// lets make padding data that is already sized correctly *very* fast
		if (data.length % blockSizeBytes == 0) {
			return data;
		}
		
		// -1 not really needed, because % blockSizeBytes has already returned, but whatever
		final int blocks = (data.length - 1) / blockSizeBytes + 1;
		
		// create the new, padded byte array containing the calculated number of blocks
		final byte[] newData = new byte[blocks * blockSizeBytes];
		System.arraycopy(data, 0, newData, 0, data.length);
		return newData;
	}

	public static String tohex(final byte[] bytes) {
		final StringBuilder sb = new StringBuilder(bytes.length * 2);
		for (int i = 0; i < bytes.length; i++) {
			sb.append(String.format("%02X", bytes[i] & UNSIGNED_BYTE_MASK));
		}
		return sb.toString();
	}

	public static String toFormattedHex(final byte[] bytes, final int tabs) {
		final StringBuilder tabsSB = new StringBuilder(tabs);
		for (int i = 0; i < tabs; i++) {
			tabsSB.append("\t");
		}
		String tabsStr = tabsSB.toString();
		
		final StringBuilder sb = new StringBuilder(bytes.length * 4);
		for (int i = 0; i < bytes.length; i++) {
			if (i % 16 == 0) {
				sb.append(tabsStr);
			}
			sb.append(String.format("%02X", bytes[i] & UNSIGNED_BYTE_MASK));
			if (i != 0 && (i + 1) % 16 == 0) {
				sb.append(String.format("%n"));
			} else {
				sb.append(" ");
			}
			
		}
		return sb.toString();
	}

	
	
	public static byte[] fromhex(String hex) {
		final byte[] result = new byte[hex.length() / 2];
		for (int i = 0; i < result.length; i++) {
			result[i] = (byte) Integer.parseInt(hex.substring(i * 2, i * 2 + 1), 16);
		}
		return result;
	}
	
	public static String tohex(long[] longs) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < longs.length; i++) {
			sb.append(String.format("0x%016X", longs[i]));
			if (i != longs.length - 1) {
				sb.append(", ");
				if (i % 4 == 3) {
					sb.append(String.format("%n"));
				}
			} 
		}

		sb.append(String.format("%n"));

		return sb.toString();
	}

    public static void printRound(final int d, long[] values) {
//    	Set<Long> orderedValues = new TreeSet<Long>();
//    	for (Long value : values) {
//			orderedValues.add(value);
//		}

    	System.out.printf("Round %d%n", d);
    	for (Long value : values) {
    		System.out.printf("%016X%n", value);
		}
    	System.out.println();
    }
}
