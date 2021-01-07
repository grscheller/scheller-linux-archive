/*
 * raw01.c
 *
 * To begin, lets keep terminal in canonical mode.
 * This mode is akso known as "cooked" mode.
 *
 * The terminal does not pass any input to the
 * program until the user presses the '<enter>'
 * key.
 *
 */

#include <unistd.h>

int main()
{
    char c;
    while (read(STDIN_FILENO, &c, 1) == 1);
    return 0;
}

/* This program will accept input until the user
 * presses either '^C' or '^D'.
 *
 *   ^C will cause a signal to be sent to terminate
 *      the process immediately.
 *
 *   ^D will signal EOF which causes read to return 0
 *
 *   ^S will signal terminal driver "terminal line
 *      discipline?" to stop sending output.
 *
 *   ^Q will send the buffered output and resume
 *      sending output.
 *
 *   ^Z will send a signal to that will cause a shell
 *      with "job control" to make the current process
 *      a "background process."
 *
 * Note: On Arch Linux, '^D' must be on a new line by
 *       itself, otherwise the user must press it twice.
 *       "Something" swallows the first one before the
 *       program can get it.
 *
 * Note: '^D' cause an EOF, but EOF is NOT a character,
 *       it is a condition the affects the read function.
 *
 * Note: From Wikipedia, "[t]he [terminal] line discipline
 *       glues the low level device driver code with the high
 *       level generic interface routines (such as read(2),
 *       write(2) and ioctl(2)), and is responsible for
 *       implementing the semantics associated with the
 *       device.  The policy is separated from the device
 *       driver so that the same serial hardware driver can
 *       be used by devices that require different data
 *       handling." 
 *
 */
