#include <stdio.h>

#define SD_NOTICE  "<5>"

int main(void)
{
    /* Example from the sd-daemon(3) manpage */
    fprintf(stderr, SD_NOTICE "Hello World!\n");

    /* C concatenates adjacent string litterals */
    fprintf(stdout, "Hello " "World" "!\n");

    char *foo = "one" " two" " three\n";
    fprintf(stdout, foo);

    char *bar = "\nSome very long string "
                "which is\n"
                "stretched over many, many "
                "lines\n"
                "can be broken apart.\n\n"
                "Without line continuations "
                "to boot!\n";
    fprintf(stdout, bar);

    return 0;
}
