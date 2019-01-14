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

#ifndef _APUE2_H
#define _APUE2_H

#define _POSIX_C_SOURCE 200809L // Compile to POSIX.1-2008 standard
#if defined(SOLARIS)
#define _XOPEN_SOURCE 600       // Use XSI 6 extention for Solaris 10
#else
#define _XOPEN_SOURCE 700       // Otherwise use XSI 7 extention
#endif

#include <sys/types.h>      // some systems still require this
#include <sys/stat.h>
#include <sys/termios.h>    // for winsize

#if defined(MACOS) || !defined(TIOCGWINSZ)
#include <sys/ioctl.h>
#endif

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

/* Determining runtime limits */
char *path_alloc(size_t *);  // Dynamically allocate space for pathname.
long open_max(void);         // Determining number of file descriptors.

__END_DECLS

#endif    // _APUE2_H 
