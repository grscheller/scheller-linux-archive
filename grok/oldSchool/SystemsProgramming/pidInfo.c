/*
 * Print out information on current process.
 *
 *  1. Actually using glib.c wrappers for
 *     underlying Linux versions of these
 *     calls.
 *  2. The unistd.h header seems to be used
 *     oto select a particular POSIX standard.
 *  3. According to the man page, the getpid,
 *     getppid, and getpgrp functions shall
 *     always be successful and no return value
 *     is reserved to indicate an error.
 */

#include <unistd.h>
#include <stdio.h>

int
main(void)
{
    printf("Process ID: %d\n", getpid());
    printf("Parent Process ID: %d\n", getppid());
    printf("Process Group ID: %d\n", getpgrp());

    return 0;
}
