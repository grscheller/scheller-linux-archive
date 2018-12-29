/* Header file for Error and Logging functions
 *
 * Purpose: 1. Set up standards used.
 *          2. Include frequently used headers - speeds up
 *             compilation via the include/spHeaders.h.gch
 *             compiler generated precompiled header.
 *          3. Prototypes for common error handling routines.
 *
 * IMPORTANT: To be included before all other header files.
 *
 */

#ifndef _SP_HEADERS_H
#define _SP_HEADERS_H

#define _POSIX_C_SOURCE 200809L // Compile to POSIX.1-2008 standard
#define _XOPEN_SOURCE 700       // with X/Open 7 extentions.

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#define MAXLINE 4096

__BEGIN_DECLS

/* Error Handling Functions */
void err_cont(int, const char *, ...);
void err_dump(const char *, ...) __attribute__((noreturn));
void err_exit(int, const char *, ...) __attribute__((noreturn));
void err_msg(const char *, ...);
void err_quit(const char *, ...) __attribute__((noreturn));
void err_ret(const char *, ...);
void err_sys(const char *, ...) __attribute__((noreturn));

__END_DECLS

#endif /* _SYSTEMS_PROGRAMMING_HEADERS_H  */
