/*
 *  Care must be taken when comparing optimization levels of code.
 *
 *  With gcc, compiling with -O2 optimization will completely
 *  "optimize" the loop away.  Essentually, all the program
 *  does is print a constant.  The calculation happens in the
 *  compiler.
 *
 *  Marking the variable sum as volatile will prevent this behavior.  
 * 
 */

#include <stdio.h>

int main(void) {

#ifdef VOLATILE
    volatile long sum = 0;
    printf("VOLATILE  is set: ");
#else
    long sum = 0;
    printf("VOLATILE not set: ");
#endif

    for(long ii = 1; ii <= 100000000L; ++ii) {
        sum += ii;
    }
    printf("%ld\n", sum);

    return 0;
}
