#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  if (argc == 2) {
      char rlpath[PATH_MAX];
      if (realpath(argv[1], rlpath) == NULL) exit(2);
      fprintf(stdout, "%s\n", rlpath);
      exit(0);
  } else {
      exit(1);
  }
}
