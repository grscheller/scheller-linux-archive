/*
 * Program to determine if input arguments,
 * parsed as unsigned longs, are prime.
 *
 * The purpose of this program is to
 * 1. Get a lambda to work by using an object of functional designed.
 * 2. Get parallism to work in Java.
 * 3. Test different build schemes.
 *
 * Note: Java does not seem to implement unsigned types.
 *
 * @author Geoffrey Scheller
 *
 */

import java.util.stream.LongStream;

public class IsPrime {

  public static void main(String[] args) {

    final int argCnt = args.length;
    long[] potPrimes = new long[argCnt];

    if (argCnt > 0) {
        try {
            for (int ii = 0; ii < argCnt; ii++) {
                long potentialPrime = Long.parseUnsignedLong(args[ii]);
                if (potentialPrime < 0L) {
                    throw new IllegalArgumentException(
                      args[ii] + " too large to fit into signed long"
                    );
                } else {
                    potPrimes[ii] = potentialPrime;
                }
            }
        } catch (Exception e) {
            System.out.println(e);
            return;
        }
    } else {
        System.out.println("No arguments given");
        return;
    }

    for (int jj = 0; jj < argCnt; jj++) {
        long potPrime = potPrimes[jj];
        System.out.print(potPrime);
        if (potPrime == 0L || potPrime == 1L) {
            System.out.println(" is not prime");
        } else if (isPrime(potPrimes[jj])){
            System.out.println(" is prime");
        } else {
            System.out.println(" is not prime");
        }
    }
  }

  private static boolean isPrime(final Long num) {
      final long upto = (long)java.lang.Math.sqrt(num) + 1L;

      return LongStream.range(2, upto)
                       .parallel()
                       .noneMatch(e -> num%e == 0);
  }

}
