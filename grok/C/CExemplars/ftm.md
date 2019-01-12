## Feature Test Macros
* Code [featureTestMacros.c](featureTestMacros.c) is from
  the `man -s7 feature_test_macros` manpage
* These CPP options need to either be defined on compile line or
  defined in source files before any system #include lines.

Compile and test your systems under various settings:
```
   $ make ftm
   $ make testSys
   $ make clean
```
Example run:
```
    $ make testSys
    gcc -Wall -o ftmBasic featureTestMacros.c
    gcc -Wall -D_XOPEN_SOURCE=500 -o ftmXOpen500 featureTestMacros.c
    gcc -Wall -D_XOPEN_SOURCE=600 -o ftmXOpen600 featureTestMacros.c
    gcc -Wall -D_XOPEN_SOURCE=700 -o ftmXOpen700 featureTestMacros.c
    gcc -Wall -D_XOPEN_SOURCE -o ftmXOpen featureTestMacros.c
    gcc -Wall -D_IOSC99 -o ftmISOC99 featureTestMacros.c
    gcc -Wall -D_IOSC11_SOURCE -o ftmISOC11 featureTestMacros.c
    gcc -Wall -D_POSIX_C_SOURCE -o ftmPOSIX featureTestMacros.c
    gcc -Wall -D_POSIX_C_SOURCE=1 -o ftmPOSIX1 featureTestMacros.c
    gcc -Wall -D_POSIX_C_SOURCE=2 -o ftmPOSIX2 featureTestMacros.c
    gcc -Wall -D_POSIX_C_SOURCE=199309L -o ftmPOSIX199309L featureTestMacros.c
    gcc -Wall -D_POSIX_C_SOURCE=199506L -o ftmPOSIX199506L featureTestMacros.c
    gcc -Wall -D_POSIX_C_SOURCE=200112L -o ftmPOSIX200112L featureTestMacros.c
    gcc -Wall -D_POSIX_C_SOURCE=200809L -o ftmPOSIX200809L featureTestMacros.c
    gcc -Wall -D_POSIX_C_SOURCE=202012L -o ftmPOSIX202012L featureTestMacros.c
    gcc -Wall -D_GNU_SOURCE -o ftmGNU featureTestMacros.c
    for bb in ftmBasic ftmXOpen500 ftmXOpen600 ftmXOpen700 ftmXOpen ftmISOC99 ftmISOC11 ftmPOSIX ftmPOSIX1 ftmPOSIX2 ftmPOSIX199309L ftmPOSIX199506L ftmPOSIX200112L ftmPOSIX200809L ftmPOSIX202012L ftmGNU;\
    do\
        echo -ne "\n$bb:\n";\
        ./$bb;\
    done

    ftmBasic:
    _POSIX_SOURCE defined
    _POSIX_C_SOURCE defined: 200809L
    _DEFAULT_SOURCE defined
    _ATFILE_SOURCE defined

    ftmXOpen500:
    _POSIX_SOURCE defined
    _POSIX_C_SOURCE defined: 199506L
    _XOPEN_SOURCE defined: 500

    ftmXOpen600:
    _POSIX_SOURCE defined
    _POSIX_C_SOURCE defined: 200112L
    _XOPEN_SOURCE defined: 600

    ftmXOpen700:
    _POSIX_SOURCE defined
    _POSIX_C_SOURCE defined: 200809L
    _XOPEN_SOURCE defined: 700
    _ATFILE_SOURCE defined

    ftmXOpen:
    _POSIX_SOURCE defined
    _POSIX_C_SOURCE defined: 2L
    _XOPEN_SOURCE defined: 1

    ftmISOC99:
    _POSIX_SOURCE defined
    _POSIX_C_SOURCE defined: 200809L
    _DEFAULT_SOURCE defined
    _ATFILE_SOURCE defined

    ftmISOC11:
    _POSIX_SOURCE defined
    _POSIX_C_SOURCE defined: 200809L
    _DEFAULT_SOURCE defined
    _ATFILE_SOURCE defined

    ftmPOSIX:
    _POSIX_C_SOURCE defined: 1L

    ftmPOSIX1:
    _POSIX_C_SOURCE defined: 1L

    ftmPOSIX2:
    _POSIX_C_SOURCE defined: 2L

    ftmPOSIX199309L:
    _POSIX_C_SOURCE defined: 199309L

    ftmPOSIX199506L:
    _POSIX_C_SOURCE defined: 199506L

    ftmPOSIX200112L:
    _POSIX_C_SOURCE defined: 200112L

    ftmPOSIX200809L:
    _POSIX_C_SOURCE defined: 200809L
    _ATFILE_SOURCE defined

    ftmPOSIX202012L:
    _POSIX_C_SOURCE defined: 202012L
    _ATFILE_SOURCE defined

    ftmGNU:
    _POSIX_SOURCE defined
    _POSIX_C_SOURCE defined: 200809L
    _ISOC99_SOURCE defined
    _ISOC11_SOURCE defined
    _XOPEN_SOURCE defined: 700
    _XOPEN_SOURCE_EXTENDED defined
    _LARGEFILE64_SOURCE defined
    _DEFAULT_SOURCE defined
    _ATFILE_SOURCE defined
    _GNU_SOURCE defined
```
