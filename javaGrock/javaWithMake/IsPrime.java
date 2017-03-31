/*
 * The purpose of this program is to try to get lambdas to
 * work by using an object which is designed to be a bit
 * more functional.
 *
 * Program to determine if input arguments,
 * parsed as unsigned longs, are prime.
 *
 * Note: Java does not seem to implement unsigned types.
 *
 * @author Geoffrey Scheller
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
              args[ii] + " too large to fit in a long"
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
      System.out.print(potPrimes[jj]);
      if (isPrime(potPrimes[jj])) {
        System.out.println(" is prime.");
      } else {
        System.out.println(" is not prime.");
      }
    }
  }

  private static boolean isPrime(final Long num) {
    final long upto = (long) java.lang.Math.sqrt(num);

    return num > 1L && 
    LongStream.range(2, upto).noneMatch(e -> num%e == 0);
  }

}
