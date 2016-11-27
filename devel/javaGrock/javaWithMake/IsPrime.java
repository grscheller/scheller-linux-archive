/*
 * The purpose of this program is to try to get lambdas to
 * work by using an object designed to be a bit more functional.
 *
 * Program to determine if input arguments are prime.
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
          potPrimes[ii] = Integer.parseInt(args[ii]);
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
      System.out.print(potPrimes[jj] + " is");
      if (isPrime(potPrimes[jj])) {
        System.out.println(" prime.");
      } else {
        System.out.println(" not prime.");
      }
    }
  }

  private static boolean isPrime(final Long num) {
    return num > 1L && 
    LongStream.range(2, num).noneMatch(e -> num%e == 0);
  }

}
