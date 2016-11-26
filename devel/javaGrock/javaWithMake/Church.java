/*
 * Sole purpose of this project is to reverse engineer the boiler plate 
 * the Netbeans IDE give me and learn how to extend it to do something
 * non-trivial in the Java language.
 *
 * This time see how to use Lambda expressions
 *
 * @author Geoffrey Scheller
 */

import java.util.function.BiFunction;

public class Church {

  // No matter what I do, I am not tickling java8
  // correctly for it to be happy with lambda expressions.
  //
  BiFunction<String, String, String> add_twoL =
   (first, second) -> first + second;

  // Vanilla static method
  static String add_twoM(String first, String second) {
    return first + second;
  }

  public static void main(String[] args) {

    if (args.length != 2) {
      System.out.println("Exactly 2 arguments needed.");
      System.exit(0);
    }
    String x = args[0];
    String y = args[1];

    // Works
    System.out.println(add_twoM(x, y));

    // Fails, looking for a method by that name
    // System.out.println(add_twoL(x, y));
  }
}
