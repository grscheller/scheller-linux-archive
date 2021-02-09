//     {{{ comments
/* Program raw07
 *
 * This program will accept input until the user
 * types the character 'q'.
 *
 * It displays the ascii numeric value of each byte it reads
 * and if the character is not a control character, it prints
 * the character too.
 *
 */ // }}}

// {{{ include files 
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
// }}}

// {{{ termibal handling
struct termios orig_termios;

void disableRawMode()
{ // {{{ code
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
} // }}}

void enableRawMode()
{ // {{{ code
    tcgetattr(STDIN_FILENO, &orig_termios);
    atexit(disableRawMode);

    struct termios raw = orig_termios;
    raw.c_iflag &= ~(IXON);
    raw.c_lflag &= ~(ECHO | ICANON | ISIG);
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
} // }}}
// }}}

// {{{ main
int main()
{ // {{{ code
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
} // }}}
// }}}

/* Notes {{{ 
 *
 * Introducing folding - otherwise no change from raw06.c
 *
 * Using Vim merge folding, to turn on use
 *
 *   :set foldmethod=marker
 *
 * Advantage: folding info is bundled with the the text file
 * Disadvatage: the embedded symbols in comments look ugly
 *
 *   zo    open one fold under cursor
 *   zO    open all folds under cursor
 *   zc    close one fold under cursor
 *   zC    close all folds under cursor
 *   za    open/close one fold
 *   zA    open/close all folds
 *   zR    open all folds in buffer
 *   zM    close all folds in buffer
 *
 * I think I will use manual folding where the folding info
 * gets stored in shada files.
 *
 */ 
// }}}
