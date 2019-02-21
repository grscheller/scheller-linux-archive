/*
 * Create a file with a "hole" in it
 */
#include "apue.h"
#include <fcntl.h>

char buf1[] = "abcdefghij";
char buf2[] = "ABCDEFGHIJ";

int
main(void)
{
    int fd_hole;
    int fd_nohole;

    if ((fd_hole = creat("file.hole", FILE_MODE)) < 0)
        err_sys("Failed to create file.hole");
    if ((fd_nohole = creat("file.nohole", FILE_MODE)) < 0)
        err_sys("Failed to create file.nohole");

    if (write(fd_hole, buf1, 10) != 10)
        err_sys("buf1 write errror to file.hole");
    if (write(fd_nohole, buf1, 10) != 10)
        err_sys("buf1 write errror to file.nohole");

    /* Offset file.hole by 16384 */
    if (lseek(fd_hole, 16384, SEEK_SET) == -1)
        err_sys("lseek error for file.hole");

    if (write(fd_hole, buf2, 10) != 10)
        err_sys("buf1 write errror to file.hole");
    if (write(fd_nohole, buf2, 10) != 10)
        err_sys("buf1 write errror to file.nohole");

    exit(EXIT_SUCCESS);
}
