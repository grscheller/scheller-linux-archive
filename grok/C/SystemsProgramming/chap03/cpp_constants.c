/*
 * List constants from fcntl.h
 */
#include "apue2.h"
#include <errno.h>
#include <fcntl.h>

static void pr_pathconf(char*, char*, int);

int
main(int argc, char *argv[])
{
    if (argc != 2)
        err_quit("usage: sysLimits <dirname>");

    /* For open and openat system calls */
    printf("O_RDONLY = %d\n", O_RDONLY);
    printf("O_WRONLY = %d\n", O_WRONLY);
    printf("O_RDWR = %d\n", O_RDWR);

    #ifdef O_EXEC
    printf("O_EXEC = %d\n", O_EXEC);
    #else
    printf("O_EXEC not defined\n");
    #endif

    #ifdef O_SEARCH
    printf("O_SEARCH = %d\n", O_SEARCH);
    #else
    printf("O_SEARCH not defined\n");
    #endif

    printf("O_APPEND = %d\n", O_APPEND);
    printf("O_CLOEXEC = %d\n", O_CLOEXEC);
    printf("O_CREAT = %d\n", O_CREAT);
    printf("O_EXCL = %d\n", O_EXCL);
    printf("O_NOCTTY = %d\n", O_NOCTTY);
    printf("O_NOFOLLOW = %d\n", O_NOFOLLOW);
    printf("O_NONBLOCK = %d\n", O_NONBLOCK);

    #ifdef O_NDELAY
    printf("O_NDELAY = %d\n", O_NDELAY);
    #else
    printf("O_NDELAY not defined\n");
    #endif

    printf("O_SYNC = %d\n", O_SYNC);
    printf("O_TRUNC = %d\n", O_TRUNC);

    #ifdef O_TTY_INIT
    printf("O_TTY_INIT = %d\n", O_TTY_INIT);
    #else
    printf("O_TTY_INIT not defined\n");
    #endif

    #ifdef O_DSYNC
    printf("O_DSYNC = %d\n", O_DSYNC);
    #else
    printf("O_DSYNC not defined\n");
    #endif

    #ifdef O_RSYNC
    printf("O_RSYNC = %d\n", O_RSYNC);
    #else
    printf("O_RSYNC not defined\n");
    #endif

    /* Special value for fd for openat meaning cwd */
    printf("\n");
    #ifdef AT_FDCWD
    printf("AT_FDCWD = %d\n", AT_FDCWD);
    #else
    printf("AT_FDCWD not defined\n");
    #endif

    /* See if filename truncation happens for argv[1] */
    printf("\n");
    #ifdef _PC_NO_TRUNC
    pr_pathconf("_POSIX_NO_TRUNC =", argv[1], _PC_NO_TRUNC);
    #else
    printf("no symbol for _PC_NO_TRUNC\n");
    #endif

    exit(EXIT_SUCCESS);
}

static void
pr_pathconf(char *mesg, char *path, int name)
{
    long val;

    fputs(mesg, stdout);
    errno = 0;
    if ((val = pathconf(path, name)) < 0) {
        if (errno != 0) {
            if (errno == EINVAL)
                fputs(" (not supported)\n", stdout);
            else
                err_sys("pathconf error, path = %s", path);
        } else {
            fputs(" (no limit)\n", stdout);
        }
    } else {
        printf(" %ld\n", val);
    }
}

/*
 * Factoids:
 * 1. On Arch Linux with kernel 4.20.0,
 *      O_NONBLOCK = O_NDELAY = 2048
 *     `O_SYNC = O_RSYNC = 1052672
 *      O_EXEC O_SEARCH, O_TTY_INIT are not defined
 */
