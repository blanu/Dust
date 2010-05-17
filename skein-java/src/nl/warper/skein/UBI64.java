package nl.warper.skein;

import static nl.warper.skein.Util.*;
import nl.warper.threefish.ThreefishImpl;
import nl.warper.threefish.ThreefishSecretKey;

/**
 * Implementation of the UBI function that can be used to chain the three parts of the Skein hash algorithm together. This is a strictly
 * 64 bit version (long in Java) for performance reasons. 
 * @author maartenb
 * @author $Author: $
 * @since 5 nov 2008
 * @version $Revision: $
 */
public class UBI64 {

	private final ThreefishImpl blockCipher;
	private long[] hi; // the intermediate result in bytes (maybe long)

	/**
	 * Constructs a UBI64 instance, currently only with the Threefish block cipher. The idea is that any tweakable block cipher may
	 * be used, but this has not been implemented at the time of writing.
	 * @param blockCipher
	 */
	public UBI64(final ThreefishImpl blockCipher) {
		if (blockCipher == null) {
			throw new IllegalArgumentException("You need to supply an underlying blockcipher");
		}
		this.blockCipher = blockCipher;
	}
	
	/**
	 * Initialize the UBI64 chaining method using the initial value. The value should be set to all zeros to initialize this instance for use with the
	 * Simple Skein hash algorithm. Warning: for performance reasons, the values of g will be directly used with the underlying block cipher, which may
	 * replace these values with other values.  
	 * @param g the uncloned block to ubi
	 */
	public void init(final long[] g) {
		final int nb = blockCipher.getBlockSize() / Long.SIZE;
		if (g == null || g.length != nb) {
			throw new IllegalArgumentException("G input parameter must be an array of " + nb + " bytes");
		}
		hi = g;    // set H{0, i}
	}
	
	/**
	 * Initializes UBI64 with a newly created block with 0h values, as used for the simple Skein hash method 
	 */
	public void init() {
		final int nb = blockCipher.getBlockSize() / Long.SIZE;
		hi = new long[nb]; // set H{0} to all zero's
	}
	
	public void update(long[] mi, long[] ti) {
		blockCipher.init(new ThreefishSecretKey(hi), ti);
		blockCipher.blockEncrypt(mi, hi);
		for ( int i = 0; i < hi.length; i++ ) {
			hi[i] ^= mi[i];
		}
	}

	public long[] getOutput() {
		return hi.clone();
	}
	
	@Override
	public String toString() {
		return String.format(" --- UBI state ---%n") + tohex(hi);
	}
}
