package nl.warper.threefish;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.crypto.SecretKey;

/**
 * Very early initial implementation of the Threefish encryption algorithm. For performance reasons,
 * this class is definately <strong>not thead safe</strong>.
 * @author maartenb
 * @author $Author: $
 * @since 4 nov 2008
 * @version $Revision: $
 */
public class ThreefishImpl {
	private static final Logger logger = Logger.getLogger(ThreefishImpl.class.getName());
	
	private static void logBlock(final Level logLevel, final String identifier, final String counterType, final int counter, final long[] block) {
		StringBuilder sb = new StringBuilder();
		if(counterType == null) {
			sb.append(String.format(" --- %s --- %n", identifier, counterType, counter));
		} else {
			sb.append(String.format(" --- %s (%s = %d) --- %n", identifier, counterType, counter));
		}
		for (int i = 0; i < block.length; i++) {
			sb.append(String.format("0x%016X", block[i]));
			if (i != block.length - 1) {
				sb.append(", ");
				if (i % 4 == 3) {
					sb.append(String.format("%n"));
				}
			} 
		}
		sb.append(String.format("%n"));
		logger.log(logLevel, sb.toString());
	}
	
	
	// === GENERAL CONSTANTS ===
	
	private static final long EXTENDED_KEY_SCHEDULE_CONST = 6148914691236517205L;
	
	public static final int BLOCK_SIZE_BITS_256 = 256;
	public static final int BLOCK_SIZE_BITS_512 = 512;
	public static final int BLOCK_SIZE_BITS_1024 = 1024;
	
	private static final int ROUNDS_72 = 72;
	private static final int ROUNDS_80 = 80;
	
	private static final int WORDS_4 = 4;
	private static final int WORDS_8 = 8;
	private static final int WORDS_16 = 16;

	private static final int TWEAK_VALUES = 3;
	private static final int SUBKEY_INTERVAL = 4;
	
	// === VALUES FOR THE WORD PERMUTATION ===

	/**
	 * Word permutation constants for PI(i) for Nw = 4.  
	 */
	private static final int[] PI4 = { 0, 3, 2, 1 };
	/**
	 * Word permutation constants for PI(i) for Nw = 8.  
	 */
	private static final int[] PI8 = { 2, 1, 4, 7, 6, 5, 0, 3 };
	/**
	 * Word permutation constants for PI(i) for Nw = 16.  
	 */
	private static final int[] PI16 = { 0, 9, 2, 13, 6, 11, 4, 15, 10, 7, 12, 3, 14, 5, 8, 1 }; 

	// === VALUES FOR THE REVERSE WORD PERMUTATION ===

	/**
	 * Reverse word permutation constants for PI(i) for Nw = 4.  
	 */
	private static final int[] RPI4 = { 0, 3, 2, 1 }; // note: RPI4 == PI4
	/**
	 * Reverse word permutation constants for PI(i) for Nw = 8.  
	 */
	private static final int[] RPI8 = { 6, 1, 0, 7, 2, 5, 4, 3 };
	/**
	 * Reverse word permutation constants for PI(i) for Nw = 16.  
	 */
	private static final int[] RPI16 = { 0, 15, 2, 11, 6, 13, 4, 9, 14, 1, 8, 5, 10, 3, 12, 7 }; 
	
	// === ROTATION CONSTANTS FOR THE MIX FUNCTION ===

	private static final int DEPTH_OF_D_IN_R = 8;
	/**
	 * Rotational constants Rd,j for Nw = 4.
	 */
	private static final int[][] R4 = {
		{  5, 56 }, 
		{ 36, 28 },
		{ 13, 46 },
		{ 58, 44 },
		{ 26, 20 },
		{ 53, 35 },
		{ 11, 42 },
		{ 59, 50 }
	};
	
	/**
	 * Rotational constants Rd,j for Nw = 8.
	 */
	private static final int[][] R8 = {
		{ 38, 30, 50, 53 },
		{ 48, 20, 43, 31 },
		{ 34, 14, 15, 27 },
		{ 26, 12, 58,  7 },
		{ 33, 49,  8, 42 },
		{ 39, 27, 41, 14 },
		{ 29, 26, 11,  9 },
		{ 33, 51, 39, 35 }
	};

	/**
	 * Rotation constants Rd,j for Nw = 16.
	 */
	private static final int[][] R16 = {
		{ 55, 43, 37, 40, 16, 22, 38, 12 },
		{ 25, 25, 46, 13, 14, 13, 52, 57 },
		{ 33,  8, 18, 57, 21, 12, 32, 54 },
		{ 34, 43, 25, 60, 44,  9, 59, 34 },
		{ 28,  7, 47, 48, 51,  9, 35, 41 },
		{ 17,  6, 18, 25, 43, 42, 40, 15 },
		{ 58,  7, 32, 45, 19, 18,  2, 56 },
		{ 47, 49, 27, 58, 37, 48, 53, 56 },
	};

	// === FIELDS CREATED DURING INSTANTIATION FOR PERFORMANCE REASONS ===
	
	private final long[] t = new long[TWEAK_VALUES]; // initial tweak words including t2
	private final long[] x = new long[2];
	private final long[] y = new long[2];

	// === FINAL FIELDS DETERMINED BY BLOCKSIZE === 
	
	private final int blockSize; // block size (in bits)
	private final int nr; // number of rounds depending on block size
	
	// === FIELDS DETERMINED BY KEY SIZE DURING INIT() ===
	
	private long[] k; // initial key words including knw
	private int nw; // number of key words excluding knw 
	private int[] pi; // word permutation pi (depends on number of words <=> block size)
	private int[] rpi; // reverse word permutation rpi (depends on number of words <=> blocksize)
	private int[][] r; // rotational constants (depends on number of words <=> block size)

	// === FIELDS DETERMINED BY KEY SIZE DURING INIT() FOR PERFORMANCE REASONS ===
	
	// NOTE next fields use lazy instantiation
	// NOTE performance/memory: can we even use the same array? let's not before testing
	private long[] vd;
	private long[] ed; 
	private long[] fd; 
	private long[] ksd;
	
	/**
	 * Threefish implementation using the specified blocksize in bits.
	 * @param blockSize either 256, 512 or 1024 (bits)
	 */
	public ThreefishImpl(final int blockSize) {
		this.blockSize = blockSize;
		
		switch(blockSize) {
		case BLOCK_SIZE_BITS_256:
		case BLOCK_SIZE_BITS_512:
			nr = ROUNDS_72;
			break;
		case BLOCK_SIZE_BITS_1024:
			nr = ROUNDS_80;
			break;
		default:
			throw new IllegalArgumentException("Illegal blocksize, use 256, 512 or 1024 bit values as blocksize");
		}
	}

	/**
	 * Threefish implementation using the specified blocksize in bits, specifying the number of rounds directly instead
	 * of using the default number of rounds depending on the blockSize. Mainly used for (performance) testing
	 * purposes.
	 * @param blockSize either 256, 512 or 1024 (bits)
	 * @param rounds the number of rounds 1..2^31
	 */
	public ThreefishImpl(final int blockSize, final int rounds) {
		this.blockSize = blockSize;
		
		switch(blockSize) {
		case BLOCK_SIZE_BITS_256:
		case BLOCK_SIZE_BITS_512:
		case BLOCK_SIZE_BITS_1024:
			break;
		default:
			throw new IllegalArgumentException("Illegal blocksize, use 256, 512 or 1024 bit values as blocksize");
		}
		
		if (rounds <= 0 || rounds % 4 != 0 ) { // DEBUG or <= 0?
			throw new IllegalArgumentException("Number of rounds should be at least 1 and should be a multiple of 4");
		}
		
		nr = rounds;
	}

	
	public void init(final SecretKey key, final long[] tweak) {
		if (!(key instanceof ThreefishSecretKey)) {
			throw new IllegalArgumentException("Only ThreeFishSecretKeys can be used for the Threefish cipher");
		}
		
		ThreefishSecretKey tfKey = (ThreefishSecretKey) key;
		final long[] k = new long[tfKey.getKeySizeInWords()];
		
		// set key values including K{N{w}}
		tfKey.getKeyWords(k);

		init(k, tweak);
	}

	/**
	 * Initialize the cipher using the key and the tweak value.
	 * @param key the Threefish key to use
	 * @param tweak the tweak values to use
	 */
	public void init(final long[] key, final long[] tweak) {
		logger.log(Level.FINER, String.format(" === Starting Threefish (blocksize = %d, rounds = %d) === %n", blockSize, nr)); 
		
		final int newNw = key.length;
		
		// only create new arrays if the value of N{w} changes (different key size)
		if (nw != newNw) {
			nw = newNw;
			
			switch(nw) {
			case WORDS_4:
				pi = PI4;
				rpi = RPI4;
				r = R4;
				break;
			case WORDS_8:
				pi = PI8;
				rpi = RPI8;
				r = R8;
				break;
			case WORDS_16:
				pi = PI16;
				rpi = RPI16;
				r = R16;
				break;
			default:
				throw new RuntimeException("Internal error: invalid threefish key");
			}
			
			this.k  = new long[nw + 1];
	
			// instantiation of these fields here for performance reasons
			vd = new long[nw]; // v is the intermediate value v{d} at round d 
			ed = new long[nw]; // ed is the value of e{d} at round d
			fd = new long[nw]; // fd is the value of f{d} at round d
			ksd = new long[nw]; // ksd is the value of k{s} at round d 
		}

		for (int i = 0; i < key.length; i++) {
			this.k[i] = key[i];
		}
		
		long knw = EXTENDED_KEY_SCHEDULE_CONST;
		for (int i = 0; i < nw; i++) {
			knw ^= this.k[i];
		}
		this.k[nw] = knw;
		logBlock(Level.FINER, "keys k", null, 0, this.k);
		
		// set tweak values
		t[0] = tweak[0];
		t[1] = tweak[1];
		t[2] = t[0] ^ t[1];
		logBlock(Level.FINER, "tweaks t", null, 0, t);
		
	}
	
	/**
	 * Implementation of the E(K, T, P) function.
	 * The K and T values should be set previously using the init() method. This version is the 64 bit implementation
	 * of Threefish.
	 * @param p the initial plain text
	 * @param c the final value defined as value v{d} where d = N{r} 
	 */
	public void blockEncrypt(final long[] p, final long[] c) {
		logBlock(Level.FINER, "plain p{i}", null, 0, p);
		// initial value = plain
		for (int i = 0; i < nw; i++) {
			vd[i] = p[i];
		}
		
		for (int d = 0; d < nr; d++) { // do the rounds
			// calculate e{d,i}
			if (d % SUBKEY_INTERVAL == 0) {
				final int s = d / SUBKEY_INTERVAL; 
				
				keySchedule(s); 
				logBlock(Level.FINER, "subkeys k{s,d}", "s", s, ksd);
				
				for (int i = 0; i < nw; i++) {
					ed[i] = vd[i] + ksd[i];
				}
			} else {
				for (int i = 0; i < nw; i++) {
					ed[i] = vd[i];
				}
			}
			logBlock(Level.FINEST, "e{d, i}", "d", d, ed);

			logger.log(Level.FINEST, String.format("d %% DEPTH_OF_D_IN_R : %d%n", d % DEPTH_OF_D_IN_R ));

			for (int j = 0; j < nw / 2; j++) {
				x[0] = ed[j * 2];
				x[1] = ed[j * 2 + 1];
				
				mix(j, d);
				
				fd[j * 2] = y[0];
				fd[j * 2 + 1] = y[1];
			}
			logBlock(Level.FINEST, "f{d, i}", "d", d, fd);
			
			for (int i = 0; i < nw; i++) {
				vd[i] = fd[pi[i]];
			}
			
			logBlock(Level.FINEST, "v{d, i}", "d", d, vd);
		}

		// do the last keyschedule
		keySchedule(nr / SUBKEY_INTERVAL); 
		logBlock(Level.FINER, "subkeys k{s,d}", "s", nr / SUBKEY_INTERVAL, ksd);

		for (int i = 0; i < nw; i++) {
			c[i] = vd[i] + ksd[i];
		}
		logBlock(Level.FINER, "result c{i}", null, 0, c);
	}
	
	/**
	 * Implementation of the MIX function.
	 * @param j the index in the rotation constants
	 * @param d the round
	 * @return JAVADOC .
	 */
	private void mix(final int j, final int d) {
		y[0] = x[0] + x[1];
		final long rotl = r[d % DEPTH_OF_D_IN_R][j];
		// java left rotation for a long
		y[1] = (x[1] << rotl) | (x[1] >>> (Long.SIZE - rotl));
		y[1] ^= y[0];
	}

	
	/**
	 * Implementation of the D(K, T, C) function.
	 * The K and T values should be set previously using the init() method. This version is the 64 bit implementation
	 * of Threefish.
	 * @param c the cipher text 
	 * @param p the plain text
	 */
	public void blockDecrypt(final long[] c, final long[] p) {
		logBlock(Level.FINER, "encrypted c{i}", null, 0, c);

		
		// initial value = plain
		for (int i = 0; i < nw; i++) {
			vd[i] = c[i];
		}
		
		for (int d = nr; d > 0; d--) { // do the rounds
			// calculate e{d,i}
			if (d % SUBKEY_INTERVAL == 0) {
				final int s = d / SUBKEY_INTERVAL;
				keySchedule(s); // calculate same keys
				logBlock(Level.FINER, "subkeys k{s,d}", "s", s, ksd);
				
				for (int i = 0; i < nw; i++) {
					fd[i] = vd[i] - ksd[i];
				}

			} else {
				for (int i = 0; i < nw; i++) {
					fd[i] = vd[i];
				}
			}
			logBlock(Level.FINEST, "f{d, i}", "d", d, fd);

			for (int i = 0; i < nw; i++) {
				ed[i] = fd[rpi[i]];
			}
			logBlock(Level.FINEST, "e{d, i}", "d", d, ed);
			
			logger.log(Level.FINEST, String.format("d %% DEPTH_OF_D_IN_R : %d%n", d % DEPTH_OF_D_IN_R ));

			for (int j = 0; j < nw / 2; j++) {
				y[0] = ed[j * 2];
				y[1] = ed[j * 2 + 1];
				
				demix(j, d - 1);
				
				vd[j * 2] = x[0];
				vd[j * 2 + 1] = x[1];
			}
			logBlock(Level.FINEST, "v{d, i}", "d", d, vd);
		}

		// do the first keyschedule
		keySchedule(0); 
		logBlock(Level.FINER, "subkeys k{s,d}", "s", 0, ksd);
		
		for (int i = 0; i < nw; i++) {
			p[i] = vd[i] - ksd[i];
		}
		logBlock(Level.FINER, "plain p{i}", null, 0, p);
	}

	/**
	 * Implementation of the un-MIX function.
	 * 
	 */
	private void demix(final int j, final int d) {
		y[1] ^= y[0];
		final long rotr = r[d % DEPTH_OF_D_IN_R][j]; // NOTE performance: darn, creation on stack!
		// right shift
		x[1] = (y[1] << (Long.SIZE - rotr)) | (y[1] >>> rotr);
		x[0] = y[0] - x[1];
	}

	
	/**
	 * Creates the subkeys.
	 * @param s the value of the round devided by 4
	 * @return the subkeys for round Ks
	 */
	private void keySchedule(final int s) {
		for (int i = 0; i < nw; i++) {
			// just put in the main key first
			ksd[i] = k[(s + i) % (nw + 1)];
		
			// don't add anything for i = 0,...,Nw - 4
			if (i == nw - 3) { // second to last
				ksd[i] += t[s % TWEAK_VALUES];
			} else if (i == nw - 2) { // first to last
				ksd[i] += t[ (s + 1) % TWEAK_VALUES ];
			} else if (i == nw - 1) { // last
				ksd[i] += s;
			}
		}
	}

	/**
	 * Simply returns the block size in bits as set by the constructor. The block size cannot
	 * be altered after the instance has been constructed.
	 * @return the block size in bits
	 */

	public int getBlockSize() {
		return blockSize;
	}
}
