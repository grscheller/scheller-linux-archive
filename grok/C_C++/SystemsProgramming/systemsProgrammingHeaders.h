/* Header file for Error and Logging functions
 *
 * Purpose: Keep error handling activity
 *          to one line in programs.
 *
 * IMPORTANT: To be included before all
 *            standard system header files.
 *
 */

#ifndef _SYSTEMS_PROGRAMMING_HEADERS_H
#define _SYSTEMS_PROGRAMMING_HEADERS_H

#define _POSIX_C_SOURCE 200809L // Compile to POSIX.1-2008 standard
#define _XOPEN_SOURCE 700       // with X/Open 7 extentions

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#define MAXLINE 4096

/* Error Handling Functions */
void err_cont(int, const char *, ...);
void err_dump(const char *, ...) __attribute__((noreturn));
void err_exit(int, const char *, ...) __attribute__((noreturn));
void err_msg(const char *, ...);
void err_quit(const char *, ...) __attribute__((noreturn));
void err_ret(const char *, ...);
void err_sys(const char *, ...) __attribute__((noreturn));

#endif /* _SYSTEMS_PROGRAMMING_HEADERS_H  */
