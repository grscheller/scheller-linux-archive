/*
 * Program to sum long integers from 1 to n.
 *
 *   Usage: Carl n
 *
 * The purpose of this program is to
 * 1. First get a trivial java program to compile
 * 2. Gain experience with Java exceptions (all unchecked)
 * 3. Test different build schemes
 *
 * Note: The Long.parseUnsignedLong method only parses
 *       strings of base 10 digits.  Won't parse octal,
 *       hex or binary.  Also, will wrap values
 *       from 2^63 to (2^64 - 1) into negative Longs.
 *
 * @author Geoffrey Scheller
 */
public class Carl {

    public static void main(String[] args) {

        if (args.length == 0) {
            System.err.println("Error: No argument given");
            return;
        }
    
        String n_string = args[0].trim();
        long n;

        try {
            n = Long.parseUnsignedLong(n_string);
            if (n < 0 || n_string == "9223372036854775808") {
                throw( new NumberFormatException(
                             "Parsed unsigned input as signed long <= 0"));
            } else if (n > 4294967295L) {
                throw( new RuntimeException(
                             "Result would be too large for 64-bit signed Long"));
            } else if (n == 0L) {
                throw( new RuntimeException(
                             "Argument must be greater than 0"));
            }
        } catch (RuntimeException e) {
            System.err.println("Error: " + e.getMessage());
            return;
        }

         // Actually Gauss's teacher's algorithm
         long sum = 0L;
         for (long ii = 1; ii <= n; ii++) {
             sum = sum + ii;
         }

         System.out.println("Sum of 1 to " + n + " is " + sum);
       }

}
