/*
 * Test whether file given on cmdline is seekable
 */
#include "apue.h"
#include <fcntl.h>

const char ErrorArg[] = "Error: Wrong number of arguments\n";
const char ErrorOpen[] = "Error: Failure openning %s\n";
const char Usage[] = "Usage: fileSeekable <file>\n";

int
main(int argc, char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, ErrorArg);
        fprintf(stderr, Usage);
        exit(EXIT_FAILURE);
    }

    int fd;
    if ((fd = open(argv[1], O_RDONLY | O_NOCTTY)) == -1 ) {
        fprintf(stderr, ErrorOpen, argv[1]);
        exit(EXIT_FAILURE);
    }

    if (lseek(fd, 0, SEEK_CUR) == -1)
        printf("Cannot seek %s\n", argv[1]);
    else
        printf("%s seekable.\n", argv[1]);

    close(fd);

    exit(EXIT_SUCCESS);
}
