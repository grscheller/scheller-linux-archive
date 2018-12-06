/*
 *  Print out information on current process.
 *
 *  Note: Actually using glib.c wrappers for
 *        underlying Linux versions of these
 *        calls.  The unistd.h header seems
 *        to select a particular POSIX standard.
 *        So far, I have just going with whatever
 *        standard gcc defaults to.
 */

#include <unistd.h>
#include <stdio.h>

int
main(void)
{
    printf("Process ID: %d\n", getpid());
    printf("Parent Process ID: %d\n", getppid());
    printf("Process Group ID: %d\n", getpgrp());
}
