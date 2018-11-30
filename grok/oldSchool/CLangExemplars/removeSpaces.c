/*
 *  Illustrates:
 *
 *  1. Use of pointers as integers (not Int's!) pointing to
 *     locations in a data structure.
 *  2. Data structure returned is what is initially
 *     given.  Not some local or static variable within
 *     the function.  Not memory allocated on heap.
 *  3. O(n) computational complexity.  Very efficient.
 *  4. Reusing a data structure as opposed to manipulating
 *     the data within it.
 *
 *  Safety:
 *  1. Function has side effects.  
 *  2. Function assumes null terminated string.  Use, or
 *     implement, something like strncpy if strings are
 *     coming from unsafe sources to prevent buffer overruns.
 *  3. Data structure used not really a data structure, just
 *     primative chars layed out in memory with a "promise"
 *     that eventually we get to the NUL character.  So much
 *     for value dependent types in C.
 *
 */

#include <stdio.h>

char *removeAllSpaces(char *data) {

    char *in, *out;

    for (in = out = data; *in !='\0'; in++)
        if (*in != ' ') *out++ = *in;
    *out = '\0';

    return data;
}

int main(int argc, char *argv[]) {

    for (int ii = 1; ii < argc; ii++)
        printf("%s\n", removeAllSpaces(argv[ii]));

    return 0;
}
