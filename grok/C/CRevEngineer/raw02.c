/*
 * raw02.c
 *
 * Terminal still in canonical mode.
 *
 */

#include <unistd.h>

int main()
{
    char c;
    while (read(STDIN_FILENO, &c, 1) == 1 && c != 'q');
    return 0;
}

/* This program will accept input until the user
 * presses either '^C' or '^D' or types the character 'q'.
 *
 * Input after 'q' but before <enter> gets fed into
 * shell, but with
 * 
 *   echo 'abcqxyz' | ./raw02 
 *
 * the 'xyz' does not.
 *
 */
