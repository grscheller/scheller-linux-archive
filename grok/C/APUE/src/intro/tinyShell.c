/*
 * A tiny shell to illustrate fork, exec, and waitpid.
 */


#include "../../include/apue.h"
#include <sys/wait.h>

static void sig_int(int);  /* Signal handler */

#define PROMPT "%% "

int
main(void)
{
    char  buf[MAXLINE];
    char  *bufPt;
    pid_t pid;
    int   status;

    if (signal(SIGINT, sig_int) == SIG_ERR)
        err_sys("Signal error");

    printf(PROMPT);    /* Prints "% " as user prompt */

    while (fgets(buf, MAXLINE, stdin) != NULL ) {
        if (buf[strlen(buf) - 1] == '\n')
            buf[strlen(buf) - 1] = 0;    

        /* Skip initial spaces/tabs and re-prompt if empty */
        for (bufPt = buf; *bufPt == ' ' || *bufPt == '\t'; bufPt++);
        if (strlen(bufPt) == 0) {
            printf(PROMPT);
            continue;
        }

        /* Fork child process */
        if ((pid = fork()) < 0) {
            err_sys("fork error");
        } else if (pid == 0) {
            execlp(bufPt, bufPt, (char *)NULL);
            err_ret("Could not execute: %s", bufPt);
            exit(127);
        }

        /* Parent waits */
        if ((pid = waitpid(pid, &status, 0)) < 0)
            err_sys("waitpid error");

        printf(PROMPT);
    }

    exit(EXIT_SUCCESS);
}

void
sig_int(int signo)
{
    printf("Interrupted!!!\n");
}

/*  Notes:
 *  1. we need to convert a possible terminating '\n' in
 *     buf to a '\0', this is in addition to the one put
 *     there by fgets.
 *
 *  Factoids:
 *  1. The execlp function will look through the environment
 *     path if first argument does not include a '/'.
 *  2. The fgets function returns buf on success, and NULL on
 *     error or when end of file occurs while no characters
 *     have yet been read.  Program is not checking for error
 *     conditions.
 *
 */
