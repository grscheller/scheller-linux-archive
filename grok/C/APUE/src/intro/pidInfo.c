/*
 * Print out information on current process
 */
#include "apue.h"

int
main(void)
{
    printf("Process ID: %ld\n", (long)getpid());
    printf("Parent Process ID: %ld\n", (long)getppid());
    printf("Process Group ID: %ld\n", (long)getpgrp());

    printf("User ID: %d\n", getuid());
    printf("Group ID: %d\n", getgid());

    exit(EXIT_SUCCESS);
}

/*
 * Notes:
 * 1. The POSIX Standards do not indicate
 *    the precise size of pid_t, only that it
 *    needs to fit inside a long.  For portability,
 *    we cast the return values of these library
 *    calls to long.
 * 2. According to the man page, the getpid,
 *    getppid, getpgrp, getuid, getgid functions
 *    shall always be successful and no return values
 *    are reserved to indicate errors.
 *
 * Factoids:
 * 1. The glibc library is userr-space code that when
 *    needed calls down into the Linux Kernel.  It does
 *    this either via system calls or through a shared
 *    object called the vDSO.  Even actual Linux system
 *    calls have glibc wrappers for them,
 * 2. C is the standard defacto ABI for Linux.  Before UNIX,
 *    the ABI (Application Binary Interface) into an
 *    operating system was the hardware's assembly language.
 *    The reason C is such, is that except for isolated uses
 *    of assembly code, the Unix Kernels are written in C.
 * 3. Different implementations of C++ are usually binary
 *    incompatible.  By using 'extern "C" { ... }' directives,
 *    or foreign function interfaces in other languages,
 *    C ABI can be the lingua franca that glues code together. 
 *    Also, allows code to communicate with device drivers
 *    written in C.
 *
 * Suppositions:
 *  1. The unistd.h header seems to be used
 *     to configure particular POSIX/ISO C standards.
 */
