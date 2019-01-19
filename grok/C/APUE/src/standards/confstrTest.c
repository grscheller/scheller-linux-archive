#include "apue.h"

int
main(void)
{
    char *buff;
    size_t n;

    n = confstr(_CS_PATH, (char *)NULL, (size_t)0);
    if ((buff = malloc(n)) == NULL)
        err_sys("malloc returned NULL for buff");
    confstr(_CS_PATH, buff, n);
    printf("Default path for POSIX.2 commands = %s\n", buff);
    free(buff);

    n = confstr(_CS_GNU_LIBC_VERSION, (char *)NULL, (size_t)0);
    if ((buff = malloc(n)) == NULL)
        err_sys("malloc returned NULL for buff");
    confstr(_CS_GNU_LIBC_VERSION, buff, n);
    printf("GNU C Library Version = %s\n", buff);
    free(buff);

    n = confstr(_CS_GNU_LIBPTHREAD_VERSION, (char *)NULL, (size_t)0);
    if ((buff = malloc(n)) == NULL)
        err_sys("malloc returned NULL for buff");
    confstr(_CS_GNU_LIBPTHREAD_VERSION, buff, n);
    printf("POSIX implementation of pthread library = %s\n", buff);
    free(buff);

    n = confstr(_CS_V6_ENV, (char *)NULL, (size_t)0);
    if ((buff = malloc(n)) == NULL)
        err_sys("malloc returned NULL for buff");
    confstr(_CS_V6_ENV, buff, n);
    printf("value for _CS_V6_ENV = %s\n",  buff);
    free(buff);

    n = confstr(_CS_V7_ENV, (char *)NULL, (size_t)0);
    if ((buff = malloc(n)) == NULL)
        err_sys("malloc returned NULL for buff");
    confstr(_CS_V7_ENV, buff, n);
    printf("value for _CS_V7_ENV = %s\n",  buff);
    free(buff);

    exit(EXIT_SUCCESS);
}
