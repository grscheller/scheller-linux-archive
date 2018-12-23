## C Systems Programming
I have always admired W. Richard Stevens' books on Unix System
programming.  Back in the mid 1990's, I thought someday I would
work my way through them.  Well, I have finally gotten around to
it, in 2018.  Thanks to POSIX standards, Stevens' books are as
relevant today as they were back then.

The books being used are his
"Advanced Programming in the UNIX Environment" third edition and his
Unix Network Programming two volume set.

A common header file, included before all system headers, configures
compiling to __POSIX.1-2008__ and __X/Open 7__ standard.

* __systemsProgrammingHeaders.h__: Common header, include before system headers
* __myErrorHandlers.c__: Common error handling routines.
* __simple_ls.c__: Application to display file names in a directory.
* __myUnbufferedCat.c__: Lower level IO defined in unistd.h
* __myBufferedCat.c__: Buffered IO defined in stdio.h
* __pidInfo.c__: Process ID information
* __tinyShell.c__: Illustrates fork, exec, and waitpid flow
