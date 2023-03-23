#include <stdio.h>
#include <errno.h>

int main(int argc, char **argv) {
    FILE *fp = NULL;
    fp = fopen("this_is_perror_test.txt", "r");
    if (fp == NULL) perror("fopen");
    return 0;
}
