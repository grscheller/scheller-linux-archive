## C Systems Programming
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
* Builds executables to compiling to __POSIX.1-2008__ and __X/Open 7__ standards.
#### Make based build
* Hierarchical build
* Individual subdirectories can be built independently of each other
#### common/ directory for utility functions
* Error handling routines
* Logging routines
* Compiled to `*.o` files for now
* Eventually turn into a static library

### UNIX System Overview - Chapter 1
#### systemsProgrammingHeaders.h
* Common header, include before all other header files
#### __myErrorHandlers.c__
* Common error handling routines.
#### __simple_ls.c__
* Application to display file names in a directory.
#### __myUnbufferedCat.c__
* Lower level IO
* Defined in unistd.h
#### __myBufferedCat.c__
* Buffered IO
* Defined in stdio.h
#### __pidInfo.c__
* Process ID information
#### __tinyShell.c__
* Illustrates fork, exec, and waitpid functions

### UNIX Standardization and Implementation - Chapter 2
#### __genSysLimits.awk__
* Awk Script based on one from Stevens' book
* Generates sysLimits.c from __sysConf.sym__, __pathConf.sym__, and unistd.h 
* Makefile will generate sysLimits executable
* Executable prints values of precompiler constants and associated runtime values
