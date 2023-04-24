/*
 * Allocate the necessary space and create a formatted string.
 *
 * Example taken from release 5.08 of the Linux manpages for printf.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

char *make_message(const char *fmt, ...) {
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

int main(int argc, char *argv[]) {
    char *myMessage = NULL;
    char *name;

    if (argc > 1)
        name = argv[1];
    else
        name = "Geoffrey Scheller";

    char *formatStr = "Hello there %s, %s and %s,\nhow are you all today?\n";

    myMessage = make_message(formatStr, "Joe Blow", "Jane Doe", name);

    if (myMessage != NULL) {
        printf("%s", myMessage);
    } else {
        printf("Something went wrong.\n");
        return 1;
    }

    return 0;
}
