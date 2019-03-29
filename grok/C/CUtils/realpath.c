#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

int
main(int argc,char **argv)
{
  char rlpath[PATH_MAX];
  if (argc==2)
  {
      if (realpath(argv[1], rlpath) == NULL)
          exit(2);
      fprintf(stdout, "%s\n", rlpath);
      exit(0);
  }
  exit(1);
}
