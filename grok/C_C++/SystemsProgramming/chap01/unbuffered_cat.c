/*
 * Copy bytes fron stdin to stdout
 *  
 */

#include "../include/spHeaders.h"

#define BUFFSIZE 4192

int
main(void)
{
    char buf[BUFFSIZE];
    int n;

    while ( (n = read(STDIN_FILENO, buf, BUFFSIZE)) > 0 )
        if (write(STDOUT_FILENO, buf, n) != n)
            err_sys("write error");

    if (n < 0)
        err_sys("read error");

    exit(0);
}

/*
 * Factoids:
 * 1. Uses low level IO defined in unistd.h
 *
 */
