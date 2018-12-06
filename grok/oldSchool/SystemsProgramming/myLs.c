/* List contents of the directory given on cmdline.
 *
 *   Usage: myLs <path-to-directory>
 *
 */
#include <sys/types.h>
#include <dirent.h>

#include <stddef.h>     /* For NULL */
#include <stdlib.h>     /* For exit */
#include <stdio.h>

#include "myCommon.h"

int
main(int argc, char *argv[])
{
    if (argc != 2)
        err_quit("Usage: myLs <path-to-directory>");

    DIR *dp;
    if ( (dp = opendir(argv[1])) == NULL )
        err_sys("myLs: Can't open %s", argv[1]);

    struct dirent  *dirp;
    while ( (dirp = readdir(dp)) != NULL )
        printf("%s\n", dirp->d_name);

    closedir(dp);
    exit(0);
}
