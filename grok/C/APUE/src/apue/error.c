/*
 * System/library error number wrapper routines.
 *
 * Purpose: Keep error handling activity
 *          to one line in the programs.
 *          Sends output to stderr.
 *
 * Source code for: err_ret
 *                  err_sys
 *                  err_dump
 *                  err_cont
 *                  err_exit
 *                  err_msg
 *                  err_quit
 */
#include "apue.h"
#include <errno.h>     /* defines errno "variable," actually a macro */
#include <stdarg.h>    /* ISO C variable arguments */

static void err_doit(int, int, const char *, va_list ap);

/* Nonfatal error related to a system/library call.
 * Print a message with errno info to stderr.
 * Return to program.
 */
void
err_ret(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, errno, fmt, ap);
    va_end(ap);
}

/* Fatal error related to a system/library call.
 * Print a message with errno info to stderr.
 * Terminate program.
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

/* Fatal error related to a system/library call,
 * Print a message with errno info to stderr.
 * Dump core.
 */
void
err_dump(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, errno, fmt, ap);
    va_end(ap);

    abort();                /* Dump core and terminate */
    exit(EXIT_FAILURE);     /* Should never get here */
}

/* Nonfatal error related to a system/library call.
 * Error code passed as explicit parameter.
 * Print a message with error info to stderr.
 * Return to program.
 */
void
err_cont(int error, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, error, fmt, ap);
    va_end(ap);
}

/* Fatal error related to a system/library call,
 * Error code passed as explicit parameter,
 * Print a message with error info to stderr.
 * Terminate program.
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

/* Nonfatal error unrelated to a system/library call.
 * Print an message to stderr.
 * Return to program.
 */
void
err_msg(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(0, 0, fmt, ap);
    va_end(ap);
}

/* Fatal error unrelated to a system/library call.
 * Print an error message to stderr.
 * Terminate program.
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

/*  Print the message to stderr and return.
 *  
 *  Caller specifies "errnoflag".
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
                 strerror(error));
    strcat(buf, "\n");
    fflush(stdout);  /* in case stdout and stderr are the same */
    fputs(buf, stderr);
    fflush(NULL);    /* flush all stdio output streams */
}
