/*
 * Copy bytes fron stdin to stdout.
 */
#include "spHeaders.h"

int
main(void)
{
    int c;
    while ( (c = getc(stdin)) != EOF )
        if (putc(c, stdout) == EOF)
            err_sys("output error");

    if (ferror(stdin))
        err_sys("input error");

    exit(EXIT_SUCCESS);
}

/*
 * Factoids:
 * 1. Uses buffered interface defined in stdio,h
 */
