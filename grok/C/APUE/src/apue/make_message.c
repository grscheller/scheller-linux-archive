/*
 * Allocate the necessary space and create a formatted string.
 *
 *   1. First argument a format printf family format string
 *   2. Takes variable number of arguments
 *   3. Returns char pointer to formatted string created
 *   4. Caller responsible to free space allocated for formatted string
 *
 * Example taken from release 5.08 of the Linux manpages for printf.
 *
 *
 * Source code for: make_message
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

char *make_message(const char *fmt, ...)
{
    int n = 0;
    size_t size = 0;
    char *p = NULL;
    va_list ap;

    /* Determine required size */

    va_start(ap, fmt);
    n = vsnprintf(p, size, fmt, ap);
    va_end(ap);

    if (n < 0)
        return NULL;

    /* One extra byte for '\0' */

    size = (size_t) n + 1;
    p = malloc(size);
    if (p == NULL)
        return NULL;

    va_start(ap, fmt);
    n = vsnprintf(p, size, fmt, ap);
    va_end(ap);

    if (n < 0) {
        free(p);
        return NULL;
    }

    return p;
}
