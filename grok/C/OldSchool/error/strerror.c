#include <stdio.h>
#include <errno.h>
#include <string.h>

int main(int argc, char **argv) {
    char *test_file = "this_is_strerror_test.txt";

    FILE *fp = NULL;
    fp = fopen(test_file, "r");
    if (fp == NULL)
        fprintf(stderr, "fopen: %s -> %s\n", strerror(errno), test_file);

    return 0;
}
