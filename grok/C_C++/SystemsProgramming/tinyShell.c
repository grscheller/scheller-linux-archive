/*
 *  A tiny shell to illustrate fork, exec, and waitpid
 */

#include "systemsProgrammingHeaders.h"
#include <sys/wait.h>

#define PROMPT "%% "

int
main(void)
{
    char  buf[MAXLINE];
    pid_t pid;
    int   status;

    printf(PROMPT);    // Prints "% " as user prompt

    // fgets returns Null on error
    while (fgets(buf, MAXLINE, stdin) != NULL ) {
        if (buf[strlen(buf) - 1] == '\n')
            buf[strlen(buf) - 1] = 0;    

        if ((pid = fork()) < 0) {
            err_sys("fork error");
        } else if (pid == 0) {       // child process
            execlp(buf, buf, (char *)0);
            err_ret("could not execute: %s", buf);
            exit(127);
        }

        if ((pid = waitpid(pid, &status, 0)) < 0)
            err_sys("waitpid error");
        printf(PROMPT);
    }

    exit(0);
}
