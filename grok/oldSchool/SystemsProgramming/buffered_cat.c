/*
 *  Copy bytes fron stdin to stdout
 *
 *  Uses buffered interface defined in stdio,h
 *
 */

#include "systemsProgrammingHeaders.h"

#include <stdlib.h>    /* for exit */
#include <stdio.h>     /* for getc, putc, ferror, EOF */

int
main(void)
{
    int c;
    while ( (c = getc(stdin)) != EOF )
        if (putc(c, stdout) == EOF)
            err_sys("output error");

    if (ferror(stdin))
        err_sys("input error");

    exit(0);
}
