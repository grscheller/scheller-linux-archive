#include "apue.h"
#include <errno.h>
#include <limits.h>

void
pr_confstr(char *mesg, int name)
{
    char *buff;
    size_t n;

    fputs(mesg, stdout);
    errno = 0;
    if ((n = confstr(name, (char *)NULL, (size_t)0)) == 0) {
        if (errno != 0) {
            if (errno == EINVAL)
                fputs(" (invalid value)\n", stdout);
            else
                err_sys("confstr error");
        } else {
            fputs("not supported", stdout);
        }
    } else {
        if (errno != 0)
            err_sys("comfstr error, returned non-zero");
        if ((buff = malloc(n)) == NULL)
            err_sys("malloc returnec NULL");
        errno = 0;
        if (confstr(name, buff, n) == 0)
            err_sys("comstr error when getting value");
        printf(" %s\n", buff);
        free(buff);
    }
}

void
pr_sysconf(char *mesg, int name)
{
    long  val;

    fputs(mesg, stdout);
    errno = 0;
    if ((val = sysconf(name)) < 0) {
        if (errno != 0) {
            if (errno == EINVAL)
                fputs(" (not supported)\n", stdout);
            else
                err_sys("sysconf error");
        } else {
            fputs(" (no limit)\n", stdout);
        }
    } else {
        printf(" %ld\n", val);
    }
}

void
pr_pathconf(char *mesg, char *path, int name)
{
    long val;

    fputs(mesg, stdout);
    errno = 0;
    if ((val = pathconf(path, name)) < 0) {
        if (errno != 0) {
            if (errno == EINVAL)
                fputs(" (not supported)\n", stdout);
            else
                err_sys("pathconf error, path = %s", path);
        } else {
            fputs(" (no limit)\n", stdout);
        }
    } else {
        printf(" %ld\n", val);
    }
}
