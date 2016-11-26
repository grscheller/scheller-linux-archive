/*
 * Sole purpose of this project is to reverse engineer the boiler plate 
 * the Netbeans IDE give me and learn how to extend it to do something
 * non-trivial in the Java language.
 *
 * Program to sum long integers from 1 to N.
 *
 * @author Geoffrey Scheller
 */
public class Carl {

    public static void main(String[] args) {
        long N = 100L;
        if (args.length > 0) {
            try {
                N = Integer.parseInt(args[0]);
            } catch (Exception e) {
                System.out.println(e);
                return;
            }
        }
        long sum = 0L;
        for (long ii = 1; ii <= N; ii++) {
            sum = sum + ii;
        }
        System.out.println("Sum of 1 to " + N + " is " + sum);
    }
    
}
