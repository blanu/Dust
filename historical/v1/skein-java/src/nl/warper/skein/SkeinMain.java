/*
 * Copyright (c) 2008 - $Date: 2008/05/29 13:50:21 $, Sdu Identification BV
 * Classificatie: Commercieel vertrouwelijk
 *
 * File:     $rcsfile$
 * Date:     $Date: 2008/05/29 13:50:21 $
 * Version:  $Revision: 1.1 $
 */
package nl.warper.skein;

import static nl.warper.skein.Util.*;

import java.security.GeneralSecurityException;
import java.security.SecureRandom;
import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import nl.warper.threefish.ThreefishImpl;
import nl.warper.threefish.ThreefishSecretKey;

/**
 * SkeinMain is used to test the SimpleSkein, UBI64 and Threefish implementations and to see that the test vectors, including initial chaining
 * vectors are correct. The output of the main() method has been used to successfully confirm the old and new chaining values as created
 * by the Skein team.
 * <P>
 * The initial implementation incorrectly used the blocksize as size of the configuration encoding instead of the
 * real size of the configuration encoding. You can simulate this incorrect behaviour by simply setting the private USE_BLOCK_SIZE constant
 * to true.
 * <P>
 * The Threefish part of the protocol is of course the most difficult part of the algorithm to implement. To test the intermediate values
 * of your own implementation, just change the logger settings below. For full logging to the console, just set the logging level
 * to Level.FINEST instead of Level.OFF.
 * @author maartenb
 * @author $Author: $
 * @since 5 nov 2008
 * @version $Revision: $
 */
public class SkeinMain {
	private static final boolean USE_BLOCK_SIZE = false;
	private static final boolean INITIAL_CHAINING_VALUES_IN_JAVA = false;

	static {
		Logger threefishLogger = Logger.getLogger(ThreefishImpl.class.getName());
	}

	public static void testThreefish(final int blockSize, final int rounds) {
		final ThreefishImpl impl;
		if(rounds <= 0) {
			impl = new ThreefishImpl(blockSize);
		} else {
			impl = new ThreefishImpl(blockSize, rounds);
		}

		byte[] keyData = new byte[blockSize / Byte.SIZE]; // initialized to 00h values, used later on
		final long[] tweak;
		try {
			SecureRandom rnd = SecureRandom.getInstance("SHA1PRNG");
			rnd.nextBytes(keyData);
			tweak = new long[] { rnd.nextLong(), rnd.nextLong() };
		} catch (GeneralSecurityException e) {
			throw new RuntimeException(e);
		}

		ThreefishSecretKey sk = new ThreefishSecretKey(keyData);

		impl.init(sk, tweak);
		final byte[] plain = "Maarten".getBytes();
		final byte[] plainPadded = zeroPad(plain, impl.getBlockSize());

		final long[] encryptedBlock = new long[impl.getBlockSize() / Long.SIZE];

		final long[] plainBlock = lsbBytesToArrayOfLong(plainPadded);
		System.out.printf("Threefish plainbl: %s%n", tohex(lsbArrayOfLongToBytes(plainBlock)));

		// not needed: impl.init(sk, tweak);
		impl.blockEncrypt(plainBlock, encryptedBlock);

		System.out.printf("Threefish encrypt: %s%n", tohex(lsbArrayOfLongToBytes(encryptedBlock)));


		long[] decryptedBlock= new long[encryptedBlock.length];
		impl.blockDecrypt(encryptedBlock, decryptedBlock);

		System.out.printf("Threefish decrypt: %s%n", tohex(lsbArrayOfLongToBytes(decryptedBlock)));
	}

	public static byte[] doSimpleSkein(final int blockSize, final byte[] message, final int outputSize) {
		final int blockSizeBytes = blockSize / Byte.SIZE;

		if(message == null) {
			throw new IllegalArgumentException("Please provide a message, even one of 0 bytes to process");
		}

		if(outputSize <= 0 || outputSize % Byte.SIZE != 0) {
			throw new IllegalArgumentException("The output size N must fullfil N MOD 8 = 0 (a complete number of bytes)");
		}

		// create buffer
		byte[] blockBuffer = new byte[blockSizeBytes];

		// create cipher
		ThreefishImpl threefish = new ThreefishImpl(blockSize);

		// create and init UBI
		UBI64 ubi = new UBI64(threefish);
		ubi.init();

		// create configuration
		Skein.Configuration config = new Skein.Configuration(outputSize, 0, 0, 0);
		byte[] configEncoding = config.getEncoded();

		// padded automatically, block is still filled with 00h values
		System.arraycopy(configEncoding, 0, blockBuffer, 0, configEncoding.length);
		long[] blockWords = lsbBytesToArrayOfLong(blockBuffer);

		// create tweak for configuration
		// used configEncoding.length, but it seems the entire block should be in the tweak value (???) -> see question on site
		int configSize = configEncoding.length;

		Skein.Tweak tweak = new Skein.Tweak(true, true, Skein.Tweak.T_CFG, false, 0, configSize);

		final long configTweak[] = { tweak.getT0(), tweak.getT1() };

		// update UBI with configuration
    //System.out.println("simple update: "+blockWords+" "+configTweak[0]+" "+configTweak[1]);
		ubi.update(blockWords, configTweak);

		// process message in blocks
		int bytesProcessed = 0;

		int messageLength = message.length;
		while (bytesProcessed < messageLength) {
			final int available = messageLength - bytesProcessed;
			final int toblock = Math.min(blockSizeBytes, available);
			System.arraycopy(message, bytesProcessed, blockBuffer, 0, toblock);

			// pad block itself (not the bits)
			if(toblock != blockSizeBytes) {
				for (int i = toblock; i < blockSizeBytes; i++) {
					blockBuffer[i] = 0;
				}
			}

			blockWords = lsbBytesToArrayOfLong(blockBuffer);

			// already update bytesProcessed (needed for tweak)
			bytesProcessed += toblock;

      //System.out.println("simple tweak: "+bytesProcessed+" "+messageLength);
			tweak = new Skein.Tweak(bytesProcessed == messageLength, bytesProcessed <= blockSizeBytes, Skein.Tweak.T_MSG, false, 0, bytesProcessed);

			// finally do the actual update
      //System.out.println("simple update: "+blockWords+" "+tweak.getT0()+" "+tweak.getT1());
			ubi.update(blockWords, new long[] { tweak.getT0(), tweak.getT1() } );
		}

		final int outputBlocks = (outputSize - 1) / blockSize + 1;

		// create a new set of longs of the same size (terrible hack, but whatever)
		long[] inputForOutput = new long[blockWords.length];
		for (int i = 0; i < outputBlocks; i++) {
			// create input for the OUTPUT function
			inputForOutput[0] = i;

			tweak = new Skein.Tweak(i == outputBlocks - 1, i == 0, Skein.Tweak.T_OUT, false, 0, 8);

			ubi.update(inputForOutput, new long[] { tweak.getT0(), tweak.getT1() } );
		}

		final long[] outputWords = ubi.getOutput();

		final byte[] output = lsbArrayOfLongToBytes(outputWords);

		return output;
	}

  private static void update(UBI64 ubi, byte[] blockBuffer, boolean last, boolean first, int type, byte[] data, int offset, int count)
  {
    final int available = data.length - offset;
    final int toblock = Math.min(blockBuffer.length, available);
		System.arraycopy(data, offset, blockBuffer, 0, toblock);

    // pad block itself (not the bits)
    if(toblock != blockBuffer.length)
    {
      for (int i = toblock; i < blockBuffer.length; i++)
      {
        blockBuffer[i] = 0;
      }
    }

		long[] blockWords = lsbBytesToArrayOfLong(blockBuffer);

		Skein.Tweak tweak = new Skein.Tweak(last, first, type, false, 0, count);

		// update UBI with configuration
    //System.out.println("full update: "+blockWords+" "+tweak.getT0()+" "+tweak.getT1());
		ubi.update(blockWords, new long[] { tweak.getT0(), tweak.getT1() });
  }

	public static byte[] fullSkein(final int blockSize, final byte[] key, final byte[] pers, final byte[] pk, final byte[] kdf, final byte[] nonce, final byte[] message, final int outputSize) {
		final int blockSizeBytes = blockSize / Byte.SIZE;

		if(outputSize <= 0 || outputSize % Byte.SIZE != 0) {
			throw new IllegalArgumentException("The output size N must fullfil N MOD 8 = 0 (a complete number of bytes)");
		}

		// create buffer
		byte[] blockBuffer = new byte[blockSizeBytes];

		// create cipher
		ThreefishImpl threefish = new ThreefishImpl(blockSize);

		// create and init UBI
		UBI64 ubi = new UBI64(threefish);
		ubi.init();

    if(key!=null)
    {
      update(ubi, blockBuffer, true, true, Skein.Tweak.T_KEY, key, 0, key.length);
    }

    if(pers!=null)
    {
      update(ubi, blockBuffer, true, true, Skein.Tweak.T_PRS, pers, 0, pers.length);
    }

    if(pk!=null)
    {
      update(ubi, blockBuffer, true, true, Skein.Tweak.T_PK, pk, 0, pk.length);
    }

    if(kdf!=null)
    {
      update(ubi, blockBuffer, true, true, Skein.Tweak.T_KDF, kdf, 0, kdf.length);
    }

    if(nonce!=null)
    {
      update(ubi, blockBuffer, true, true, Skein.Tweak.T_NON, nonce, 0, nonce.length);
    }

		// create configuration
		Skein.Configuration config = new Skein.Configuration(outputSize, 0, 0, 0);
		byte[] configEncoding = config.getEncoded();

		// create tweak for configuration
    update(ubi, blockBuffer, true, true, Skein.Tweak.T_CFG, configEncoding, 0, configEncoding.length);

    if(message!=null)
    {
      // process message in blocks
      int bytesProcessed = 0;
      while (bytesProcessed < message.length)
      {
        int available = message.length - bytesProcessed;
        int toblock = Math.min(blockBuffer.length, available);
        //System.out.println("full tweak: "+(bytesProcessed+toblock)+" "+message.length);
        update(ubi, blockBuffer, bytesProcessed+toblock == message.length, bytesProcessed+toblock <= blockSizeBytes, Skein.Tweak.T_MSG, message, bytesProcessed, bytesProcessed+toblock);
        bytesProcessed += toblock;
      }
    }

		final int outputBlocks = (outputSize - 1) / blockSize + 1;

		// create a new set of longs of the same size (terrible hack, but whatever)
		long[] blockWords = lsbBytesToArrayOfLong(blockBuffer);
		long[] inputForOutput = new long[blockWords.length];
		for (int i = 0; i < outputBlocks; i++)
    {
			// create input for the OUTPUT function
			inputForOutput[0] = i;

			Skein.Tweak tweak = new Skein.Tweak(i == outputBlocks - 1, i == 0, Skein.Tweak.T_OUT, false, 0, 8);

			ubi.update(inputForOutput, new long[] { tweak.getT0(), tweak.getT1() } );
		}

		final long[] outputWords = ubi.getOutput();

		final byte[] output = lsbArrayOfLongToBytes(outputWords);

		return output;
	}

	public static void showConfigurationInit(final int blockSize, final int outputSize) {

		final int blockSizeBytes = blockSize / Byte.SIZE;

		if(outputSize <= 0 || outputSize % Byte.SIZE != 0) {
			throw new IllegalArgumentException("The output size N must fullfil N MOD 8 = 0 (a complete number of bytes)");
		}

		// create buffer
		byte[] blockBuffer = new byte[blockSizeBytes];

		// create cipher
		ThreefishImpl threefish = new ThreefishImpl(blockSize);

		// create and init UBI
		UBI64 ubi = new UBI64(threefish);
		ubi.init();

		// create configuration
		Skein.Configuration config = new Skein.Configuration(outputSize, 0, 0, 0);
		byte[] configEncoding = config.getEncoded();
		// padded automatically, block is still filled with 00h values
		System.arraycopy(configEncoding, 0, blockBuffer, 0, configEncoding.length);
		long[] blockWords = lsbBytesToArrayOfLong(blockBuffer);

		int configSize = configEncoding.length;
		if(USE_BLOCK_SIZE) { // wrong, but whatever
			configSize = blockSizeBytes;
		}


		// create tweak for configuration
		// used configEncoding.length, but it seems the entire block should be in the tweak value (???) -> see question on site
		Skein.Tweak tweak = new Skein.Tweak(true, true, Skein.Tweak.T_CFG, false, 0, configSize);

		final long configTweak[] = { tweak.getT0(), tweak.getT1() };

		// update UBI with configuration
		ubi.update(blockWords, configTweak);
		long[] initialChainingValue = ubi.getOutput();

		if (INITIAL_CHAINING_VALUES_IN_JAVA) {
			System.out.printf("\tpublic static final long[] INITIAL_CHAINING_VALUE_SKEIN_%d_%d = {%n", blockSize, outputSize);

			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < initialChainingValue.length; i++) {
				sb.append(String.format("0x%016XL", initialChainingValue[i]));
				if (i != initialChainingValue.length - 1) {
					sb.append(", ");
					if (i % 4 == 3) {
						sb.append(String.format("%n"));
					}
				}
			}
			sb.append(String.format("%n"));
			System.out.println(sb.toString());
		} else {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < initialChainingValue.length; i++) {
				sb.append(String.format("0x%016X", initialChainingValue[i]));
				if (i != initialChainingValue.length - 1) {
					sb.append(", ");
					if (i % 4 == 3) {
						sb.append(String.format("%n"));
					}
				}
			}
			System.out.println(sb.toString());

		}
	}

	private static byte[] createTestArray(int bytes) {
		byte[] testArray = new byte[bytes];
		for(int i = 0; i < bytes; i++) {
			testArray[i] = (byte) (- i - 1);
		}
		return testArray;
	}

	/**
	 * @param args JAVADOC .
	 */
	public static void main(final String ... args) {
		System.out.println();
		System.out.println(" === Threefish encrypt and decyrpt === ");
		testThreefish(256, 72);
		testThreefish(512, 80);
		testThreefish(1024, 80);

		System.out.println();
		System.out.println(" === initial chaining values === ");
		System.out.printf("B.1 Skein-256-128%n%n");
		showConfigurationInit(256, 128);
		System.out.println();

		System.out.printf("B.2 Skein-256-128%n%n");
		showConfigurationInit(256, 160);
		System.out.println();

		System.out.printf("B.3 Skein-256-224%n%n");
		showConfigurationInit(256, 224);
		System.out.println();

		System.out.printf("B.4 Skein-256-256%n%n");
		showConfigurationInit(256, 256);
		System.out.println();

		System.out.printf("B.5 Skein-512-128%n%n");
		showConfigurationInit(512, 128);
		System.out.println();

		System.out.printf("B.6 Skein-512-160%n%n");
		showConfigurationInit(512, 160);
		System.out.println();

		System.out.printf("B.7 Skein-512-224%n%n");
		showConfigurationInit(512, 224);
		System.out.println();

		System.out.printf("B.8 Skein-512-256%n%n");
		showConfigurationInit(512, 256);
		System.out.println();

		System.out.printf("B.9 Skein-512-384%n%n");
		showConfigurationInit(512, 384);
		System.out.println();

		System.out.printf("B.10 Skein-512-512%n%n");
		showConfigurationInit(512, 512);
		System.out.println();

		System.out.printf("B.11 Skein-1024-384%n%n");
		showConfigurationInit(1024, 384);
		System.out.println();

		System.out.printf("B.12 Skein-1024-512%n%n");
		showConfigurationInit(1024, 512);
		System.out.println();

		System.out.printf("B.13 Skein-1024-1024%n%n");
		showConfigurationInit(1024, 1024);
		System.out.println();

		// initialization
		byte[] data;
		int blockSize;
		int outputSize;
		int length;
		byte[] output;

		System.out.println();
		System.out.println(" === test values === ");

		// C.1
		blockSize = 256;
		outputSize = 256;

		length = 1;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("C.1 Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 32;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("C.1 Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 64;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("C.1 Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		// C.2
		blockSize = 512;
		outputSize = 512;

		length = 1;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("C.2 Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 64;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("C.2 Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 128;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("C.2 Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		// C.3
		blockSize = 1024;
		outputSize = 1024;

		length = 1;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("C.2 Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 128;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("C.2 Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 256;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("C.2 Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));
	}
}
