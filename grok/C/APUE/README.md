# C Systems Programming - APUE
I have always admired W. Richard Stevens' books on Unix System
programming.  Back in the mid 1990's, I thought someday I would
work my way through them.  Well, I have finally gotten around to
it, in 2018.  Thanks to POSIX standards, Stevens' books are as
relevant today as they were back then.

The book being used are this project is his
"Advanced Programming in the UNIX Environment" 3rd edition.
The book has a website with source code, 
[APUE](http://apuebook.com/), but the makefiles provided by
the co-author of the 3rd edition are rudimentary and don't
document the build well.  I think I can do better.

### Common Infrastructure
#### include/apue2.h
* Common header file to be included before all other header files
* Builds executables conforming to __POSIX.1-2008__ and __XSI 7__ standards
* POSIX is a portmanteau of "Portable Operating System" and "Unix"
* XSI stands for X/OPEN System Interfaces
#### src/libapue2/errorHandlers.c
* Error handling routines
* Compiled to `errorHandlers.o` and archived to lib/libapue2.a
#### src/libapue2/limits.c
* Contains routines to determine variaous systems limits at run time.
* `path_alloc` uses malloc to allocate space for pathnames
* `open_max` returns maximum number of possible open file descriptures
#### make based build
* Hierarchical build
* Individual src/ subdirectories can be built independently of each other
* make.definitions contain system specific info for build

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
* Generates __sysLimits.c__ from __sysConf.sym__ and __pathConf.sym__
* Makefile will generate sysLimits executable
* Executable prints values of precompiler constants from __unistd.h__
* Also prints associated runtime values

### File I/O - Chapter 3
#### `cpp_constants.c`
* Lists constants from __fcntl.h__ and related runtime parameters
#### `seekTest.c`
* Determine if stdin is seekable
