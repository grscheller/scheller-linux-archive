/*
 * Create a file with a "hole" in it
 */
#include "apue.h"
#include <fcntl.h>

char buf1[] = "abcdefghij";
char buf2[] = "ABCDEFGHIJ";
char    x[] = "x";

int
main(void)
{
    int fd_hole;
    int fd_nohole;
    int fd_filled;

    if ((fd_hole = creat("file.hole", FILE_MODE)) < 0)
        err_sys("Failed to create file.hole");
    if ((fd_nohole = creat("file.nohole", FILE_MODE)) < 0)
        err_sys("Failed to create file.nohole");
    if ((fd_filled = creat("file.filled", FILE_MODE)) < 0)
        err_sys("Failed to create file.filled");

    if (write(fd_hole, buf1, 10) != 10)
        err_sys("buf1 write errror to file.hole");
    if (write(fd_nohole, buf1, 10) != 10)
        err_sys("buf1 write errror to file.nohole");
    if (write(fd_filled, buf1, 10) != 10)
        err_sys("buf1 write error to file.filled");

    /* Offset file.hole by 16384 = 2^14 */
    if (lseek(fd_hole, 16384, SEEK_CUR) == -1)
        err_sys("lseek error for file.hole");
    /* Fill in file.filled by 16384 bytes */
    for (int ii=0; ii < 16384; ii++)
        if (write(fd_filled, x, 1) != 1)
            err_sys("write error to file.filled");

    if (write(fd_hole, buf2, 10) != 10)
        err_sys("buf2 write errror to file.hole");
    if (write(fd_nohole, buf2, 10) != 10)
        err_sys("buf2 write errror to file.nohole");
    if (write(fd_filled, buf2, 10) != 10)
        err_sys("buf2 write errror to file.nohole");

    exit(EXIT_SUCCESS);
}

/*
 * Notes:
 *   $ od -c file.filled 
 *   0000000   a   b   c   d   e   f   g   h   i   j   x   x   x   x   x   x
 *   0000020   x   x   x   x   x   x   x   x   x   x   x   x   x   x   x   x
 *   *
 *   0040000   x   x   x   x   x   x   x   x   x   x   A   B   C   D   E   F
 *   0040020   G   H   I   J
 *   0040024
 *
 *   $ od -c file.hole
 *   0000000   a   b   c   d   e   f   g   h   i   j  \0  \0  \0  \0  \0  \0
 *   0000020  \0  \0  \0  \0  \0  \0  \0  \0  \0  \0  \0  \0  \0  \0  \0  \0
 *   *
 *   0040000  \0  \0  \0  \0  \0  \0  \0  \0  \0  \0   A   B   C   D   E   F
 *   0040020   G   H   I   J
 *   0040024
 *
 *   $ od -c file.nohole 
 *   0000000   a   b   c   d   e   f   g   h   i   j   A   B   C   D   E   F
 *   0000020   G   H   I   J
 *   0000024
 *
 *   $ ls -ls file.*
 *   20 -rw-r----- 1 geoff geoff 16404 Feb 21 16:04 file.filled
 *    8 -rw-r----- 1 geoff geoff 16404 Feb 21 16:04 file.hole
 *    4 -rw-r----- 1 geoff geoff    20 Feb 21 16:04 file.nohole
 *
 *   $ file file.*
 *   file.filled: ASCII text, with very long lines, with no line terminators
 *   file.hole:   data
 *   file.nohole: ASCII text, with no line terminators
 */
