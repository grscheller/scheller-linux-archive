/*
 *   Traditional K&R Hello World program.
 */

#include <stdio.h>

int main(int argc, char *argv[]) {

    char *hw;
    hw = "  Hello World!";

    printf(hw+2);

    return 0;
}

/* Notes (from top to bottom):
 *
 *   1. The #include mechanism for function prototypes.
 *   2. ANSI C prototyping.
 *   3. argv is a pointer to a pointer to a char.  Basically
 *      it is a pointer to the first of argc number of char pointers
 *      laid out in memory.  These char pointers point to whereever
 *      the compiler decided to lay out the input arguments given to
 *      the program.  The compiler NUL terminates the strings so your
 *      code knows where they end.  Also, argc tells your code how
 *      many char ** have been layed out in memory.  There are absolutely
 *      no bound checks.  It may be even a mistake to think of the
 *      construct *avgv[] as being a datatype.
 *   4. Language is statement based, not expression based.
 *      Statements (runtime) and declaration (usually compile time)
 *      are terminated, not separated, by semicolens.  Programmer
 *      controls a "thread of execution" through the code.  The code
 *      being a sequence of statements.  Statements affect the
 *      state of "variables" and other computer "resources."
 *   5. Parser ignores whitespace other than to separate tokens.
 *   6. The string literal, "  Hello World!" is syntactic sugar.
 *      The string name, hw, acts like a memory pointer to the first
 *      character of the string, not the string value itself.
 *   7. Use of a standard library for IO (not language builtins).
 *   8. hw + 2 is an example of pointer arithmatic.  Note that the
 *      compiler still hides the details of the actual memory layout
 *      from the programmer.
 *   9. The return statemenat as a language builtin.
 */
