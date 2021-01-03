/*
 * raw03.c
 *
 * Turn off echoing.  First steps to raw mode.
 *
 */

#include <termios.h>
#include <unistd.h>

void enableRawMode()
{
    struct termios raw;

    tcgetattr(STDIN_FILENO, &raw);
    raw.c_lflag &= ~(ECHO);
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

int main()
{
    enableRawMode();

    char c;
    while (read(STDIN_FILENO, &c, 1) == 1 && c != 'q');
    return 0;
}

/* This program will accept input until the user
 * presses either ^C or ^D or types the character 'q'.
 *
 * Input after 'q' but before <enter> gets fed into
 * shell, but
 * 
 *   echo 'abcqxyz' | ./raw03 
 *
 * the 'xyz' does not.  The echo command may receive
 * a signal before it prints them out?
 *
 * Terminal is left with echo off.  Use the /usr/bin/reset
 * command to rest the terminal.
 */
