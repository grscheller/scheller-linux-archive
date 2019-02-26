/*
 * Atomically copy bytes fron stdin to stdout
 */
#include "apue.h"
#include <fcntl.h>

#define BUFFSIZE 4096

const char ErrorArg[] = "Error: Wrong number of arguments";
const char Usage[] = "Usage: atomic_cp <file_in> <file_out>";

int
main(int argc, char *argv[])
{
    if (argc != 3)
        err_quit("%s\n%s", ErrorArg, Usage);

    int fd_in;
    if ((fd_in = open(argv[1], O_RDONLY | FILE_MODE)) == -1 )
        err_sys("Failed to open %s", argv[1]);

    int fd_out;
    if ((fd_out = open(argv[2], O_WRONLY | O_CREAT)) == -1 )
        err_sys("Failed to open %s", argv[2]);

    char buf[BUFFSIZE];
    ssize_t n;
    off_t offset = 0;

    while ((n = pread(fd_in, buf, BUFFSIZE, offset)) > 0 ) {
        if (pwrite(fd_out, buf, n, offset) != n)
            err_sys("write error");
        offset += n;
    }

    if (n < 0)
        err_sys("read error");

    exit(EXIT_SUCCESS);
}
