# include "../../include/apue.h"
#include <fcntl.h>

int
main(int argc, char *argv[])
{
    if (argc != 2)
        err_quit("Usage: testfd <file descriptor #>");
    int fd = atoi(argv[1]);

    int fs_flags;
    if ((fs_flags = fcntl(fd, F_GETFL, 0)) < 0)
        err_sys("fcntl error for fd %d", fd);

    switch (fs_flags & O_ACCMODE) {
        case O_RDONLY:
            printf("read only");
            break;
        case O_WRONLY:
            printf("write only");
            break;
        case O_RDWR:
            printf("read write");
            break;
        default:
            err_dump("unknown access mode");
    }

    if (fs_flags & O_APPEND)
        printf(", append");
    if (fs_flags & O_NONBLOCK)
        printf(", nonblocking");
    if (fs_flags & O_SYNC)
        printf(", synchronous writes");
    #if !defined(_POSIX_C_Source) && defined(O_FSYNC) && (O_FSYNC != O_SYNC)
    if (fs_flags & O_FSYNC)
        printf(", synchronous writes");
    #endif
    #if defined(O_DSYNC) && (O_FSYNC != O_SYNC)
    if (fs_flags & O_DSYNC)
        printf(", data-synchronous writes");
    #endif
    #if defined(O_RSYNC) && (O_RSYNC != O_SYNC)
    if (fs_flags & O_RSYNC)
        printf(", data-synchronous reads");
    #endif
    putchar('\n');

    exit(0);
}
