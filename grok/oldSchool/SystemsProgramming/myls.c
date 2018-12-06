#include <sys/types.h>
#include <dirent.h>

#include <stddef.h>     /* For NULL */
#include <stdlib.h>     /* For exit */
#include <stdio.h>

#include "myErrorHandlers.h"

int
main(int argc, char *argv[])
{
    DIR            *dp;
    struct dirent  *dirp;

    if (argc != 2)
        err_quit("usage: myls <path-to-directory>");

    if ( (dp = opendir(argv[1])) == NULL )
        err_sys("myls: Can't open %s", argv[1]);

    while ( (dirp = readdir(dp)) != NULL )
        printf("%s\n", dirp->d_name);

    closedir(dp);
    exit(0);

}
