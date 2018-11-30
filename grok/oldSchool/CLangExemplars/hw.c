/*
 *   Traditional K&R Hello World program.
 *
 *   Illustrates:
 *
 *   1. ANSI C prototyping.
 *   2. The #include mechanism for function prototypes.
 *   3. The return statemenat is a language builtin.
 *   4. Use of a standard library for IO (not language builtins).
 *   5. Some syntatic sugar, for instance a string litteral
 *      really is a pointed to the beginning of the string
 *      it represents.
 *   6. My lack of any attempt to make it look like
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
 *   7. Yet I still keep the argc and argv names.  The first is
 *      for "argument count" and the second "argument vector."
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
    hw = "Hello World!";

    printf("Hello World!\n");

    return 0;
}
