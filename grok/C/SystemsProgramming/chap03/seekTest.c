/*
 * Test whether standard input is seekable
 */
#include "apue2.h"

int
main(void)
{
    if (lseek(STDIN_FILENO, 0, SEEK_CUR) == -1)
        printf("cannot seek stdin\n");
    else
        printf("stdin seekable\n");

    exit(EXIT_SUCCESS);
}

/*
 * Notes:
 *   1. Since lseek above will only ever return 0 upon success, no need
 *      to check errno.
 *
 * Factoids:
 *   1. lseek & close only needs unistd.h while open needs fcntl.h
 *   2. The "l" in lseek stands for "long int" which was the traditional
 *      type befor off_t.
 *   3, lseek affects the read and write system calls
 *   4. By itself, lseek does not extend the size of a file, the
 *      write, truncate, and ftruncate system calls will.
 *   5. Extending an offset beyond size of file and calling write
 *      will result with a file "with a hole" in it.  Any bytes that
 *      have not been written to are read as 0.
 *   6. Upon error, file offset remains the same.
 *   7. The standard developers choose not to require checking errno
 *      in all situations.  Negative offsets possible, but not with
 *      regular files, block special files, or directories.  Applications
 *      using devices that can return negative offsrts need to initially 
 *      set errno=0 and then check errno to distinguish (off_t)(-1) with
 *      an error condition.
 */
