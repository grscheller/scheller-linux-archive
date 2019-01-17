/*
 * Limit allocation and determination functions
 *
 * Purpose: Determine variaous systems limits at run time.
 *
 *   1. Useful when executable compiled elsewhere.
 *   2. Some limits are indeterminate even at run time.
 *   3. Sometimes we have to make best guess.
 *
 * Source code for: path_alloc
 *                  open_max
 *
 */
#include "apue.h"
#include <errno.h>     /* defines errno "variable," actually a macro */
#include <limits.h>    /* ISO C variable arguments */

#ifdef PATH_MAX
static long pathmax = PATH_MAX;
#else
static long pathmax = 0;
#endif

static long posix_version = 0;
static long xsi_version = 0;

/* If PATH_MAX indeterminate, no guarantee this guess is adequate */
#define PATH_MAX_GUESS 1024

/* Dynamically allocate space for a pathname
 * 
 * Returns pointer to allocated space, Null upon failure.
 * Also, return size via sizep, which needs to point
 * to somewhere that exists.
 *
 * Caches previous obtained value of PATH_MAX.
 */
char *
path_alloc(size_t *sizep)
{
    char *ptr;
    size_t size;

    if (posix_version == 0)
        posix_version = sysconf(_SC_VERSION);
    if (xsi_version == 0)
        xsi_version = sysconf(_SC_XOPEN_VERSION);

    /* Check if first time called */
    if (pathmax == 0) {
        errno = 0;
        if  ((pathmax = pathconf("/", _PC_PATH_MAX)) < 0) {
            if (errno == 0)
                pathmax = PATH_MAX_GUESS;   /* indeterminate */
            else
                err_sys("pathconf error for _PC_PATH_MAX");
        } else {
            pathmax++;   /* add "1" since it's relative to "/" */
        }
    }

    /* Before POSIX.1-2001, no guarantee that PATH_MAX includes
     * the terminating null byte.  Same for XPG3.
     */
    if ((posix_version < 200112L) && (xsi_version < 4))
        size = ++pathmax;
    else
        size = pathmax;

    /* allocate space */
    if ((ptr = malloc(size)) == NULL)
        err_sys("malloc error for pathname");

    /* Return pointer to allocated space and set *sizep
     * after checking that it points somewhere valid.
     */
    if (sizep != NULL)
        *sizep = size;
    return(ptr);
}

#ifdef OPEN_MAX
static long openmax = OPEN_MAX;
#else
static long openmax = 0;
#endif

/* If PATH_MAX indeterminate, no guarantee this guess is adequate */
#define OPEN_MAX_GUESS 2048

/* Determine number of file descriptors.*/
long
open_max(void)
{
    /* Check if first time called */
    if (openmax == 0) {
        errno = 0;
        if ((openmax = sysconf(_SC_OPEN_MAX)) < 0) {
            if (errno == 0)
                openmax = OPEN_MAX_GUESS;   /* indeterminate */
            else
                err_sys("sysconf error for _SC_OPEN_MAX");
        }

        #ifdef LONG_MAX
        if (openmax == LONG_MAX)
            openmax = OPEN_MAX_GUESS;
        #endif
    }

    return openmax;
}
