/* Header file for writing portable Unix applications
 * based on the 3rd ed. of the W. Richard Stevens book
 * Advanced Programming in the UNIX Environment (APUE).
 *
 * Purpose: 1. Set up standards used via feature test macros
 *             done so in as much of a cross OS compatible
 *             way as possible
 *          2. Within a given OS, try to make executables
 *             cross platform compatible
 *          3. Include frequently used headers
 *          4. Define some convience macros
 *          5. Prototype error and logging functions
 *          6. Prototype functions to determine runtime limits
 *
 * IMPORTANT: To be included before all other header files.
 *
 */

#ifndef _APUE_H
#define _APUE_H

#define _POSIX_C_SOURCE 200809L /* Compile to POSIX.1-2008 standard */
#define _XOPEN_SOURCE 700       /* with XSI 7 extentions.           */

#if defined(BSD)
# define __BSD_VISIBLE
#endif

#if defined(MACOS)
# define _DARWIN_C_SOURCE
#endif

#include <sys/types.h>      /* some systems still require this */
#include <sys/stat.h>
#include <sys/termios.h>    /* for winsize */

#if defined(MACOS) || !defined(TIOCGWINSZ)
# include <sys/ioctl.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#define MAXLINE 4096

/* Default file and directory access permisions */
#define FILE_MODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
#define DIR_MODE  (FILE_MODE | S_IXUSR | S_IXGRP | S_IXOTH)

__BEGIN_DECLS

/* Error Handling Functions */
void err_cont(int, const char *, ...);
void err_dump(const char *, ...) __attribute__((noreturn));
void err_exit(int, const char *, ...) __attribute__((noreturn));
void err_msg(const char *, ...);
void err_quit(const char *, ...) __attribute__((noreturn));
void err_ret(const char *, ...);
void err_sys(const char *, ...) __attribute__((noreturn));

/* Error Logging Functions */
void log_open(const char *, int, int);
void log_cont(int, const char *, ...);
void log_exit(int, const char *, ...) __attribute__((noreturn));
void log_info(const char *, ...);
void log_msg(const char *, ...);
void log_quit(const char *, ...) __attribute__((noreturn));
void log_ret(const char *, ...);
void log_sys(const char *, ...) __attribute__((noreturn));

/* Determining runtime limits */
char *path_alloc(size_t *);  /* Dynamically allocate space for pathname. */
long open_max(void);         /* Determining number of file descriptors. */

/* Format a message and allocate sufficient space for it */
char *make_message(const char *, ...);

__END_DECLS

#endif    /* _APUE_H  */
