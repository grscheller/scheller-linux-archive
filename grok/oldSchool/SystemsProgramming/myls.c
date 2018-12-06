#include <sys/types.h>
#include <dirent.h>

#include <stddef.h>     /* For NULL */
#include <stdlib.h>     /* For exit */
#include <stdio.h>

#include "myErrorHandlers.h"

int
main(int argc, char *argv[])
{
    if (argc != 2)
        err_quit("usage: myls <path-to-directory>");

    DIR *dp;
    if ( (dp = opendir(argv[1])) == NULL )
        err_sys("myls: Can't open %s", argv[1]);

    struct dirent  *dirp;
    while ( (dirp = readdir(dp)) != NULL )
        printf("%s\n", dirp->d_name);

    closedir(dp);
    exit(0);
}
