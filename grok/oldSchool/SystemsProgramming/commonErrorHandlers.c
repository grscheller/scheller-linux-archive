/*
 * Error Handling functions
 *
 * Purpose: Keep error handling activity
 *          to one line in programs.
 *
 */

#include "systemsProgrammingHeaders.h"

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

static void err_doit(int, const char *, va_list ap);

/* Fatal error related to a system call,
 * print a message, dump core and terminate program
 */
void
err_dump(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, fmt, ap);
    va_end(ap);

    abort();     /* Dump core and terminate */
    exit(1);     /* Should never get here */
}

/* Nonfatal error unrelated to a system call,
 * print a message and return
 */
void
err_msg(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(0, fmt, ap);
    va_end(ap);

    return;
}

/* Fatal error unrelated to a system call,
 * print a message and terminate program
 */
void
err_quit(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(0, fmt, ap);
    va_end(ap);

    exit(1);
}

/* Nonfatal error related to a system call,
 * print a message and return
 */
void
err_ret(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, fmt, ap);
    va_end(ap);

    return;
}

/* Fatal error related to a system call,
 * print a message and terminate program
 */
void
err_sys(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, fmt, ap);
    va_end(ap);

    exit(1);
}

/*  Factor out commonality, encapsulate all the non-obvious
 *  wisdom in one place, makes implementation of public
 *  interface easier to grok.
 */
static void
err_doit(int errnoflag, const char *fmt, va_list ap)
{
    int errno_save = errno;
    char buf[MAXLINE];

    vsprintf(buf, fmt, ap);
    if (errnoflag)
        sprintf(buf+strlen(buf), ": %s", strerror(errno_save));
    strcat(buf, "\n");
    fflush(stdout);  /* in case stdout and stderr are the same */
    fputs(buf, stderr);
    fflush(NULL);    /* flush all stdio output streams */

    return;
}
