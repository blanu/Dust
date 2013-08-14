package nl.warper.skein;

import static nl.warper.skein.Util.*;
import nl.warper.skein.SkeinMain;

class SkeinTest extends SkeinMain
{
	private static byte[] createTestArray(int bytes) {
		byte[] testArray = new byte[bytes];
		for(int i = 0; i < bytes; i++) {
			testArray[i] = (byte) (- i - 1);
		}
		return testArray;
	}

	public static void main(String[] args)
  {
		// initialization
    byte[] key;
    byte[] pers;
    byte[] nonce;
		byte[] data;
		int blockSize;
		int outputSize;
		int length;
		byte[] output;

		System.out.println();
		System.out.println(" === test values === ");

		// C.1
		blockSize = 512;
		outputSize = 256;

		length = 1;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("1.a Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));
		output = fullSkein(blockSize, null, null, null, null, null, data, outputSize);
		System.out.printf("1.b Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 32;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("2.a Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));
		output = fullSkein(blockSize, null, null, null, null, null, data, outputSize);
		System.out.printf("2.b Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 64;
		data = createTestArray(length);
		output = doSimpleSkein(blockSize, data, outputSize);
		System.out.printf("3.a Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));
		output = fullSkein(blockSize, null, null, null, null, null, data, outputSize);
		System.out.printf("3.b Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 32;
		key = createTestArray(length);
		data = createTestArray(length);
		output = fullSkein(blockSize, key, null, null, null, null, data, outputSize);
		System.out.printf("4. Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 32;
		key = createTestArray(length);
		pers = "Personalized!".getBytes();
		data = createTestArray(length);
		output = fullSkein(blockSize, key, pers, null, null, null, data, outputSize);
		System.out.printf("4. Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));

		length = 32;
		key = createTestArray(length);
		pers = "Personalized!".getBytes();
    nonce = createTestArray(length);
		data = createTestArray(length);
		output = fullSkein(blockSize, key, pers, null, null, nonce, data, outputSize);
		System.out.printf("4. Skein-%d-%d T(%d) %n%nMessage data:%n%s%nResult:%n%s%n", blockSize, outputSize, length, toFormattedHex(data, 1), toFormattedHex(output, 1));
	}
}