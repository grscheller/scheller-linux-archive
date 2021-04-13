# Generates the source code for sysLimits.c which
# gets compiled into the sysLimits and sysNoLimits
# executables.
#
# Format of the C Preprocessor directives chosen to
# make source code readable in both this file and
# the generated C code.
BEGIN {
  printf("#ifndef NO_APUE\n")
  printf("  #include \"apue.h\"\n")
  printf("#else\n")
  printf("  #include <stdio.h>\n")
  printf("  #include <stdlib.h>\n")
  printf("#endif\n")
  printf("#include <unistd.h>\n")
  printf("#ifndef NO_LIMITS\n")
  printf("  #include <limits.h>\n")
  printf("#endif\n\n")
  printf("void pr_confstr(char *, int);\n")
  printf("void pr_sysconf(char *, int);\n")
  printf("void pr_pathconf(char *, char *, int);\n")
  printf("void pr_feature(char *, char, long);\n\n")
  printf("int\n")
  printf("main(int argc, char *argv[])\n")
  printf("{\n")
  printf("  if (argc != 2) {\n")
  printf("     fprintf(stderr, \"usage: sys[No]Limits <dirname>\");\n")
  printf("     exit(1);\n")
  printf("  }\n\n")
  FS="\t+"
  while (getline <"sysconf.sym" > 0) {
    if ( !/^[ \t]*#/ && !/^[ \t]*$/ ) {
        printf("  #ifdef %s\n", $1)
        printf("     printf(\"%s defined to be %%ld\\n\", (long)%s+0);\n", $1, $1)
        printf("  #else\n")
        printf("     printf(\"no symbol for %s\\n\");\n", $1)
        printf("  #endif\n")
        printf("  #ifdef %s\n", $2)
        printf("     pr_sysconf(\"%s =\", %s);\n", $1, $2)
        printf("  #else\n")
        printf("     printf(\"no symbol for %s\\n\");\n", $2)
        printf("  #endif\n\n")
    }
  }
  close("sysconf.sym")
  while (getline <"pathconf.sym" > 0) {
    if ( !/^[ \t]*#/ && !/^[ \t]*$/ ) {
        printf("  #ifdef %s\n", $1)
        printf("     printf(\"%s defined to be %%ld\\n\", (long)%s+0);\n", $1, $1)
        printf("  #else\n")
        printf("     printf(\"no symbol for %s\\n\");\n", $1)
        printf("  #endif\n")
        printf("  #ifdef %s\n", $2)
        printf("     pr_pathconf(\"%s =\", argv[1], %s);\n", $1, $2)
        printf("  #else\n")
        printf("     printf(\"no symbol for %s\\n\");\n", $2)
        printf("  #endif\n\n")
    }
  }
  close("pathconf.sym")
  while (getline <"confstr.sym" > 0) {
    if ( !/^[ \t]*#/ && !/^[ \t]*$/ ) {
        printf("  #ifdef %s\n", $1)
        printf("     pr_confstr(\"%s -> \", %s);\n", $1, $1)
        printf("  #else\n")
        printf("     printf(\"no symbol for %s\\n\");\n", $1)
        printf("  #endif\n\n")
    }
  }
  close("confstr.sym")
  while (getline <"feature.sym" > 0) {
    if ( !/^[ \t]*#/ && !/^[ \t]*$/ ) {
        printf("  #ifdef %s\n", $1)
        printf("     pr_feature(\"%s\", '%s', %s);\n", $1, $2, $1)
        printf("  #else\n")
        printf("     printf(\"no symbol for %s\\n\");\n", $1)
        printf("  #endif\n\n")
    }
  }
  close("feature.sym")
  printf("  exit(0);\n\n")
  printf("}")
  exit
}
