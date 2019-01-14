/*
 * Error Handling functions
 *
 * Purpose: Keep error handling activity
 *          to one line in the programs.
 *
 */
#include "apue2.h"
#include <errno.h>     // defines errno "variable," actually a macro
#include <stdarg.h>    // ISO C variable arguments

static void err_doit(int, int, const char *, va_list ap);

/* Nonfatal error unrelated to a system call,
 * error code passed as explicit parameter,
 * print a message and return.
 */
void
err_cont(int error, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, error, fmt, ap);
    va_end(ap);
}

/* Fatal error related to a system call,
 * print a message, dump core and terminate program.
 */
void
err_dump(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, errno, fmt, ap);
    va_end(ap);

    abort();     // Dump core and terminate
    exit(EXIT_FAILURE);     // Should never get here
}

/* Fatal error unrelated to a system call,
 * error code passed as explicit parameter,
 * print a message and terminate.
 */
void
err_exit(int error, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, error, fmt, ap);
    va_end(ap);

    exit(EXIT_FAILURE);
}

/* Nonfatal error unrelated to a system call,
 * print a message and return.
 */
void
err_msg(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(0, 0, fmt, ap);
    va_end(ap);
}

/* Fatal error unrelated to a system call,
 * print a message and terminate program.
 */
void
err_quit(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(0, 0, fmt, ap);
    va_end(ap);

    exit(EXIT_FAILURE);
}

/* Nonfatal error related to a system call,
 * print a message and return.
 */
void
err_ret(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, errno, fmt, ap);
    va_end(ap);
}

/* Fatal error related to a system call,
 * print a message and terminate program.
 */
void
err_sys(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, errno, fmt, ap);
    va_end(ap);

    exit(EXIT_FAILURE);
}

/*  Factor out commonality, encapsulate all the non-obvious
 *  wisdom in one place, makes implementation of public
 *  interface easier to grok.
 */
static void
err_doit(int errnoflag, int error, const char *fmt, va_list ap)
{
    char buf[MAXLINE];

    vsnprintf(buf, MAXLINE-1, fmt, ap);
    if (errnoflag)
        snprintf(buf+strlen(buf),
                 MAXLINE-strlen(buf)-1,
                 " - %s",
                 strerror(error) );
    strcat(buf, "\n");
    fflush(stdout);  /* in case stdout and stderr are the same */
    fputs(buf, stderr);
    fflush(NULL);    /* flush all stdio output streams */
}
