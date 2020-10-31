#include <stdio.h>

#define SD_NOTICE  "<5>"

int main(void)
{
    /* Example from the sd-daemon(3) manpage */
    fprintf(stderr, SD_NOTICE "Hello World!\n");

    /* C concatenates adjacent string litterals */
    fprintf(stdout, "Hello " "World" "!\n");

    char *foo = "one" " two" " three";
    fprintf(stdout, "%s\n", foo);

    char *bar = "Some very long string "
                "which is\n"
                "stretched over many, many "
                "lines\n"
                "can be broken apart.\n\n"
                "Without line continuations "
                "to boot!";
    fprintf(stdout, "\n%s\n", bar);

    return 0;
}
