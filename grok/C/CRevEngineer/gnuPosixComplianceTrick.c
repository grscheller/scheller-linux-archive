#include <stdio.h>
#include <unistd.h>
#include "gnuPosixComplianceTrick.h"

int
main(void)
{
    printf("foo0 = %d\n", foo0);   // macro
    printf("foo1 = %d\n", foo1);   // macro
    printf("foo2 = %d\n", foo2);   // macro
    printf("foo3 = %d\n", foo3);   // enumeration
    printf("FOO3 = %d\n", FOO3);   // macro, but with different name than the enumeration 
    printf("foo4 = %d\n", foo4);   // enumeration
    
    printf("_SC_CHILD_MAX = %d\n", _SC_CHILD_MAX);
    printf("CHILD_MAX from sysconf = %ld\n", sysconf(_SC_CHILD_MAX));

    return 0;
}
