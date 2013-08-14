package nl.warper.skein;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;

/**
 * The main class for the Skein hash algorithm by Niels Ferguson, 
 * Stefan Lucks, Bruce Schneier, Doug Whiting, Mihir Bellare, Tadayoshi Kohno,
 * Jon Callas and Jesse Walker.
 * @author Maarten Bodewes
 * @since 4 nov 2008
 */
public class Skein {
	public static class Configuration {
		private final long outputLength;
		private final int treeLeafSize;
		private final int treeFanOut;
		private final int treeMaxHeight;

		public Configuration(final long outputLength, final int treeLeafSize, final int treeFanOut, final int treeMaxHight) {
			this.outputLength = outputLength;
			this.treeLeafSize = treeLeafSize;
			this.treeFanOut = treeFanOut;
			this.treeMaxHeight = treeMaxHight;
		}

		public byte[] getEncoded() {

			try {
				final ByteArrayOutputStream baos = new ByteArrayOutputStream();

				// schema identifier
				baos.write(new byte[] { 'S', 'H', 'A', '3' });

				// version number (in LSB!!!)
				baos.write(0x01);
				baos.write(0x00);

				// reserved
				baos.write(0x00);
				baos.write(0x00);

				// output length
				baos.write(Util.lsbLongToBytes(this.outputLength));

				// output tree parts
				baos.write(this.treeLeafSize);
				baos.write(this.treeFanOut);
				baos.write(this.treeMaxHeight);

				// write reserved bytes [part deux]
				for (int i = 0; i < 13; i++) {
					baos.write(0x00);
				}

				return baos.toByteArray();
			} catch (final IOException e) {
				throw new RuntimeException(e);
			}
		}
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public String toString() {
			final StringBuilder sb = new StringBuilder();
			sb.append(String.format("Schema Identifier: SHA3 (static)%n"));			
			sb.append(String.format("Schema version: 1.0 (static)%n"));			
			sb.append(String.format("Output length (in bits): %d%n", outputLength));
			
			if(treeLeafSize == 0 && treeFanOut == 0 && treeMaxHeight == 0) {
				sb.append("Tree mode not used%n");
			} else {
				sb.append(String.format("Tree mode used (tree leaf size = %d, tree fan out = %d and tree max height = %d%n", treeLeafSize, treeFanOut, treeMaxHeight));
			}
			
			return sb.toString();
		}
	}
	
	/**
	 * Calculates the tweak values according to the various tweak fields. This class is for initial calculations only, it's too slow for general calculation of
	 * tweak values.
	 * Warning: due to lsb mode you can easily confuse tweakHigh and tweakLow. T[0] actually contains the position (0..2^64 at least) and T[1] the type etc.
	 * @author maartenb
	 * @author $Author: $
	 * @since 5 nov 2008
	 * @version $Revision: $
	 */
	public static class Tweak {
		public static final BigInteger MAXIMUM_POSITION_VALUE_PLUS_ONE = BigInteger.valueOf(2).pow(96);

		public static final int FINAL_LOCATION_IN_T1 = 127 - Long.SIZE;
		public static final int FIRST_LOCATION_IN_T1 = 126 - Long.SIZE;
		public static final int TYPE_LOCATION_IN_T1 = 120 - Long.SIZE; 
		public static final int BIT_PAD_LOCATION_IN_T1 = 119 - Long.SIZE;

		public static final int T_KEY = 0;
		public static final int T_CFG = 4;
		public static final int T_PRS = 8;
		public static final int T_PK = 12;
		public static final int T_KDF = 16;
		public static final int T_NON = 20;
		public static final int T_MSG = 48;
		public static final int T_OUT = 63;

		private final long t0;
		private final long t1;

		/**
		 * Convenience constructor that works for values in the range [0..2^64-1], encoded in the long parameter position
		 * as an unsigned value. 
		 * @param isFinal indicates this is the tweak value of the final block to be processed
		 * @param isFirst indicates this is the tweak value of the fist block to be processed
		 * @param type the type contained in the block
		 * @param bitPadded the last byte in the block uses the indicated bit padding (see class description)
		 * @param treeLevel the level in the tree of this block
		 * @param position the number of bytes processed so far including the unpadded bytes in this block
		 */
		public Tweak(final boolean isFinal, final boolean isFirst, final int type, final boolean bitPadded,
				final int treeLevel, final long position) {
			if(position < 0) {
				throw new IllegalArgumentException("Position may not be negative");
			}
			
			long highTweak = 0;
			if (isFinal) {
				highTweak |= 1L << Tweak.FINAL_LOCATION_IN_T1;
			}
			if (isFirst) {
				highTweak |= 1L << Tweak.FIRST_LOCATION_IN_T1;
			}
			highTweak |= ((long) type) << TYPE_LOCATION_IN_T1;
			if (bitPadded) {
				highTweak |= 1L << Tweak.BIT_PAD_LOCATION_IN_T1;
			}
			
			this.t1 = highTweak;
			this.t0 = position;
		}
		
		public Tweak(final boolean isFinal, final boolean isFirst, final int type, final boolean bitPadded,
				final BigInteger position) {
			if (position.signum() == -1 || position.compareTo(MAXIMUM_POSITION_VALUE_PLUS_ONE) >= 0) {
				throw new IllegalArgumentException("Position must be in the range [0..2^96-1]");
			}

			long highTweak = 0;
			if (isFinal) {
				highTweak |= 1L << Tweak.FINAL_LOCATION_IN_T1;
			}
			if (isFirst) {
				highTweak |= 1L << Tweak.FIRST_LOCATION_IN_T1;
			}
			highTweak |= ((long) type) << TYPE_LOCATION_IN_T1;
			if (bitPadded) {
				highTweak |= 1L << Tweak.BIT_PAD_LOCATION_IN_T1;
			}
			
			highTweak |= position.shiftRight(64).longValue();
			this.t1 = highTweak;
			this.t0 = position.longValue();
		}

		public Tweak(final long t0, final long t1) {
			this.t0 = t0;
			this.t1 = t1;
		}

		/**
		 * Note that because of the LSB configuration of the hash method, the low tweak value must come to the left of the 
		 * high tweak value (visualize the lowest byte containing the lowest part of the position completely to the left).  
		 * @return the lowest part of the tweak value
		 */
		public long getT0() {
			return this.t0;
		}

		/**
		 * Note that because of the LSB configuration of the hash method, the low tweak value must come to the left of the 
		 * high tweak value (visualize the lowest byte containing the lowest part of the position completely to the left).  
		 * @return the highest part of the tweak value
		 */
		public long getT1() {
			return this.t1;
		}

		public String toString() {
			return String.format("T0=%016X T1=%016X", this.t0, this.t1);
		}
	}
}
