/*
 * Detemine the type of files given on cmdline.
 */
# include "apue.h"

int
main(int argc, char *argv[])
{
    int ii;
    struct stat buf;
    char *ptr;

    for (ii = 1; ii < argc; ii++) {
       printf("%s: ", argv[ii]); 
       if (lstat(argv[ii], &buf) < 0) {
           err_ret("lstat error");
           continue;
       }
       if (S_ISREG(buf.st_mode))
           ptr = "regular";
       else if (S_ISDIR(buf.st_mode))
           ptr = "directory";
       else if (S_ISCHR(buf.st_mode))
           ptr = "character special";
       else if (S_ISBLK(buf.st_mode))
           ptr = "block special";
       else if (S_ISFIFO(buf.st_mode))
           ptr = "fifo";
       else if (S_ISLNK(buf.st_mode))
           ptr = "symbolic link";
       else if (S_ISSOCK(buf.st_mode))
           ptr = "socket";
       else
           ptr = "** unknown mode **";
       printf("%s\n", ptr);
    }

    exit(0);
}
