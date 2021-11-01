#include <stdio.h>
#include <wchar.h>
#include <stddef.h>
#include <locale.h>

int main(void)
{
    setlocale(LC_CTYPE, "en_US.utf8"); /* Matches my locale.          */
 /* setlocale(LC_CTYPE, "");              Also works, more portable?  */

    wchar_t *unicode =
        L"\u0060abc"
        L"abcXYZ"
    /*  L"\u0061\u0062\u0063\u0058\u0059\u005a"  Same as above, but
     *                                           not valid universal
     *                                           characters
     */
        L"\u03B1\u03B2\u03B3"
        L"\u2200\u2205\u2605";

    int n = fwprintf(stdout, L"%ls", unicode);
    if (n < 0) {
        wprintf(L"\nSomething went wrong!\n\n");
    } else {
        wprintf(L"\n%d characters printed\n\n", n);
    }

    wprintf(L"print lambda via universal character name: \u03bb\n");
    wprintf(L"print lambda via unicode in source code: λ\n");

    int i = 20;
    i = i + 1;
    /*
     *  Can't mix calls of wide/non-wide characters versions
     *  of printf/wprintf family of functions.  Calling printf
     *  below would fail.  Which versons are based on the
     *  first call made by the program.  The program sets
     *  up the iostreams first call.
     */
    if (i >= 10) {
        wprintf(L"\nThe ultimate answer is %d\n", 2*i);
    }

    if (i < 100) {
        wprintf(L"\nhello %ls \n", L"\u03ba\u03bb\u03bc\u03bd");
        if (n < 0) {
            wprintf(L"\nSomething went wrong!\n\n");
        } else {
            wprintf(L"\n%d characters printed\n\n", n);
        }
    }

    wchar_t *stuff = L"\u03ba\u03bb\u03bc\u03bd";
    if (i < 100) {
        wprintf(L"\nhello %ls \n", stuff);
        if (n < 0) {
            wprintf(L"\nSomething went wrong!\n\n");
        } else {
            wprintf(L"\n%d characters printed\n\n", n);
        }
    }

    return 0;
}
