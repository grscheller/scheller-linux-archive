#include <stdio.h>
#include <wchar.h>
#include <stddef.h>
#include <locale.h>

#define SD_NOTICE  "<5>"

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
    wprintf(L"\n%d characters printed\n\n", n);

    wprintf(L"print lambda via universal character name: \u03bb\n");
    wprintf(L"print lambda via unicode in source code: λ\n");

    return 0;
}
