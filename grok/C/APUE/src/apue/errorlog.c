/*
 * System/library error number wrapper functions.
 * Used by programs that can run as daemons.
 *
 * Purpose: Keep error handling activity
 *          to one line in programs.
 *          Send output to syslog().
 *
 * Note: Caller must define and set log_to_stderr
 *
 *         log_to_stderr = 0     when running as a deamon
 *         log_to_stderr = 1     when running as daemon
 *
 * Source code for: log_open
 *                  log_ret
 *                  log_sys
 *                  log_cont    (not in book)
 *                  log_exit
 *                  log_info    (not in book)
 *                  log_msg
 *                  log_quit
 */
#include "apue.h"
#include <errno.h>     /* defines errno "variable," actually a macroa */
#include <stdarg.h>    /* ISO C variable arguments */
#include <syslog.h>

static void log_doit(int, int, int, const char *, va_list ap);

extern int log_to_stderr;

/* Initialize syslog(), if running as daemon.
 */
void
log_open(const char *ident, int option, int facility)
{
    if (log_to_stderr == 0)
        openlog(ident, option, facility);
}

/* Nonfatal error related to a system/library call.
 * Log a message with errno info.
 * Return to program.
 */
void
log_ret(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    log_doit(1, errno, LOG_ERR, fmt, ap);
    va_end(ap);
}

/* Fatal error related to a system/library call,
 * Log a message with errno info.
 * Terminate program.
 */
void
log_sys(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    log_doit(1, errno, LOG_ERR, fmt, ap);
    va_end(ap);

    exit(EXIT_FAILURE);
}

/* Nonfatal error related to a system/library call.
 * Error code passed as explicit parameter.
 * Log a message with error info.
 * Return to program.
 */
void
log_cont(int error, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    log_doit(1, error, LOG_ERR, fmt, ap);
    va_end(ap);
}

/* Fatal error related to a system/library call.
 * Error code passed as explicit parameter.
 * Log a message with error info.
 * Terminate program.
 */
void
log_exit(int error, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    log_doit(1, error, LOG_ERR, fmt, ap);
    va_end(ap);

    exit(EXIT_FAILURE);
}

/* Informational message.
 * Log an informational message.
 * Return to program.
 */
void
log_info(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    log_doit(0, 0, LOG_INFO, fmt, ap);
    va_end(ap);
}

/* Nonfatal error unrelated to a system/library call.
 * Log an error message.
 * Return to program.
 */
void
log_msg(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    log_doit(0, 0, LOG_ERR, fmt, ap);
    va_end(ap);
}

/* Fatal error unrelated to a system/library call.
 * Log an error message.
 * Terminate program.
 */
void
log_quit(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    log_doit(0, 0, LOG_ERR, fmt, ap);
    va_end(ap);

    exit(EXIT_FAILURE);
}

/*  Log the message (or print to stderr).
 *
 *  Caller specifies "errnoflag" and "priority".
 */
static void
log_doit(int errnoflag, int error, int priority, const char *fmt, va_list ap)
{
    char buf[MAXLINE];

    vsnprintf(buf, MAXLINE-1, fmt, ap);
    if (errnoflag)
        snprintf(buf+strlen(buf),
                 MAXLINE-strlen(buf)-1,
                 " - %s",
                 strerror(error));
    strcat(buf, "\n");
    if (log_to_stderr) {
        fflush(stdout);  /* in case stdout and stderr are the same */
        fputs(buf, stderr);
        fflush(NULL);    /* flush all stdio output streams */
    } else {
        syslog(priority, "%s", buf);
    }
}
