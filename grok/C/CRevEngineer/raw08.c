/* Program raw08
 *
 * This program will accept input until the user
 * types the character 'q'.
 *
 * It displays the ascii numeric value of each byte it reads
 * and if the character is not a control character, it prints
 * the character too.
 *
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

struct termios orig_termios;

void disableRawMode()
{
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

void enableRawMode()
{
    tcgetattr(STDIN_FILENO, &orig_termios);
    atexit(disableRawMode);

    struct termios raw = orig_termios;
    raw.c_iflag &= ~(ICRNL | IXON);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

int main()
{
    enableRawMode();

    char c;
    while (read(STDIN_FILENO, &c, 1) == 1) {
        if (iscntrl(c)) {
            printf("%#4o\n", c);
        } else {
            printf("%#4o ('%c')\n", c, c);
        }

        if (c == 'q') return 0;
    }

    return 0;
}

/* Notes
 *
 * The IEXTEN mask turns off CTRL-V
 *   This does not seem necessaryon Arch Linux for
 *   gnome-terminal, urxvt, and linux console
 *
 * The ICRNL mask turns off translates <CR> to a <NL>
 *   This fixes the problem with CTRL-M being
 *   displayed as as CTRL-J.
 *
 */ 
