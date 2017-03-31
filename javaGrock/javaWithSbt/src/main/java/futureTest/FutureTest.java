package futureTest;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.logging.Logger;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Java program to show how to use futures in Java.
 *
 * Code initially taken from this blog:
 * http://javarevisited.blogspot.com/2015/01/how-to-use-future-and-futuretask-in-Java.html
 *
 */
public class FutureTest {

  private static final ExecutorService threadpool = Executors.newFixedThreadPool(3);

  public static void main(String args[]) throws InterruptedException, ExecutionException {

    long input = 20;

    FactorialCalculator task = new FactorialCalculator(input);

    System.out.println("Submitting Task ...");
    Future<Long> future = threadpool.submit(task);
    System.out.println("Task is submitted");

    long factorialPre = -42;
    try {
        factorialPre = future.get(18L, TimeUnit.MILLISECONDS);
    } catch (TimeoutException ex) {
        System.out.println("Timed out!!!!  ");
    }
    System.out.println("Factorial of " + input + " is " + factorialPre);

    while (!future.isDone()) {
      System.out.println("Task is not completed yet....");
      Thread.sleep(1); //sleep for 1 millisecond before checking again
    }

    System.out.println("Task is completed, let's check result");
    long factorial = future.get();
    System.out.println("Factorial of " + input + " is " + factorial);

    threadpool.shutdown();
  }

  private static class FactorialCalculator implements Callable {

    private final long number;

    public FactorialCalculator(long number) {
      this.number = number;
    }

    @Override
    public Long call() {
      long output = 0;
      try {
          output = factorial(number);
      } catch (InterruptedException ex) {
          Logger.getLogger(FutureTest.class.getName()).log(Level.SEVERE, null, ex);
      }

      return output;
    }

    private long factorial(long number) throws InterruptedException {

      if (number < 0L) {
        throw new IllegalArgumentException("Number must be greater than zero");
      }

      long result = 1; while (number > 0L) {
        Thread.sleep(1); // adding 1 millisecond delay for this example
        result = result * number;
        number--;
      }

      return result;
    }

  }

}
