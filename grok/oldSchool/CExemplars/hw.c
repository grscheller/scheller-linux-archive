/*
 *   Traditional K&R Hello World program.
 *
 *   Illustrates:
 *
 *   1. ANSI C prototyping.
 *   2. The #include mechanism for function prototypes.
 *   3. The return statemenat as a language builtin.
 *   4. Use of a standard library for IO (not language builtins).
 *   5. tring literal syntactic sugar.  String name being a memory pointer
 *      to the first character of the string, not the value itself.
 *   6. Pointer arithmatic.  Yet the compiler still hides the details
 *      of the actual memory layout from the programmer.
 *   7. My lack of any attempt to make it look "familiar" like
 *      pre-ANSI K&R C code:
 *
 *        int
 *        main (int argc, char** argv}
 *        {
 *           ...
 *
 *      mimics pre-ANSI,
 *
 *        main (argc, argv)
 *        char **argv
 *        {
 *           ...
 *
 *      where argc (and main's return type too) is assumed to
 *      be the built in type Int.
 *
 *   8. Yet retaining other social conventionsa:
 *      - argc and argv for "argument count" and "argument vector."
 *      - consistent indentation in a language where
 *        white space serves to only separate tokens
 *   9. Language is statement based, not expression based.
 *      Statements (runtime) and declaration (usually compile time)
 *      are terminated, not separated, by semicolens.  Programmer
 *      controls a "thread of execution" through the code.  The code
 *      being a sequence of statements.  Statements affect the
 *      state of "variables" and other computer "resources."
 *
 *   Note: argv is a pointer to a pointer to a char.  Basically
 *   it is a pointer to the first of argc number of char pointers
 *   laid out in memory.  These char pointers point to whereever
 *   the compiler decided to lay out the input arguments given to
 *   the program.  The compiler NUL terminates the strings so your
 *   code knows where they end.  Also, argc tells your code how
 *   many char ** have been layed out in memory.  There are absolutely
 *   no bound checks.  It may be even a mistake to think of the
 *   construct *avgv[] as being a datatype.
 */
#include <stdio.h>

int main(int argc, char *argv[]) {

    char *hw;
    hw = "  Hello World!";

    printf(hw+2);

    return 0;
}
