# C Systems Programming
I have always admired W. Richard Stevens' books on Unix System
programming.  Back in the mid 1990's, I thought someday I would
work my way through them.  Well, I have finally gotten around to
it, in 2018.  Thanks to POSIX standards, Stevens' books are as
relevant today as they were back then.

The books being used are his
"Advanced Programming in the UNIX Environment" third edition and his
Unix Network Programming two volume set.

### Common infrastructure
#### include/spHeaders.h
* Common header file to be included before all other header files
* Builds executables conforming to __POSIX.1-2008__ and __XSI 7__ standards
* POSIX is a portmanteau of "Portable Operating System" and "Unix"
* XSI stands for X/OPEN System Interfaces
#### common/spErrorHandlers.c
* Error handling routines
* Compiled to a `*.o` file for now
#### common/spLimits.c
* Contains routines to determine variaous systems limits at run time.
* `path_alloc` uses malloc to allocate space for pathnames
* `open_max` returns maximum number of possible open file descriptures
#### make based build
* Hierarchical build
* Individual subdirectories can be built independently of each other

### UNIX System Overview - Chapter 1
#### simpleLs.c
* Application to display file names in a directory.
#### myUnbufferedCat.c
* Lower level IO
* Defined in unistd.h
#### myBufferedCat.c
* Buffered IO
* Defined in stdio.h
#### pidInfo.c
* Process ID information
#### tinyShell.c
* Illustrates fork, exec, and waitpid functions

### UNIX Standardization and Implementation - Chapter 2
#### genSysLimits.awk
* Awk Script based on one from Stevens' book
* Generates sysLimits.c from __sysConf.sym__, __pathConf.sym__, and __unistd.h__
* Makefile will generate sysLimits executable
* Executable prints values of precompiler constants and associated runtime values
