/* Error and Logging functions
 *
 * Purpose: Keep handling erros and loging
 *          activity to one line in programs
 *
 */

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "myErrorHandlers.h"
static void err_doit(int, const char *, va_list ap);

/* Fatal error unrelated to a system call
 * 
 * Print a message and terminate program
 *
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

void
err_sys(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    err_doit(1, fmt, ap);
    va_end(ap);

    exit(1);
}

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
