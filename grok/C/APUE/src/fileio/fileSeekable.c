/*
 * Test whether file given on cmdline is seek-able
 */

# include "../../include/apue.h"
#include <fcntl.h>

const char ErrorArg[] = "Error: Wrong number of arguments";
const char Usage[] = "Usage: fileSeekable <file>";

int
main(int argc, char *argv[])
{
    if (argc != 2)
        err_quit("%s\n%s", ErrorArg, Usage);

    int fd;
    if ((fd = open(argv[1], O_RDONLY | O_NOCTTY)) == -1 )
        err_sys("Failed to open %s", argv[1]);

    if (lseek(fd, 0, SEEK_CUR) == -1)
        printf("%s not seekable.\n", argv[1]);
    else
        printf("%s seekable.\n", argv[1]);

    if (close(fd) == -1)
        err_sys("Failed to close file descripture on cleanup");

    exit(EXIT_SUCCESS);
}
