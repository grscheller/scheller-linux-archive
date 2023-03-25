/*
 *  Care must be taken when comparing optimization levels of code.
 *
 *  With gcc, compiling with -O2 optimization will completely
 *  "optimize" the loop away.  All the program does is print
 *  a constant, the calculation happens in the compiler.
 *
 *  Marking the variable sum as volatile will prevent this behavior.  
 * 
 */

#include <stdio.h>
#include <stdint.h>

int main(void) {
#ifdef VOLATILE
    volatile int64_t sum = 0;
    printf("VOLATILE  is set: ");
#else
    int64_t sum = 0;
    printf("VOLATILE not set: ");
#endif

    for (int64_t ii = 1; ii <= 100000000L; ++ii) {
        sum += ii;
    }
    printf("%ld\n", sum);

    return 0;
}
