# Advanced Programming in the UNIX Environment (APUE)
I have always admired W. Richard Stevens' books on Unix System
programming.  Back in the mid 1990's, I thought someday I would
work my way through them.  Well, I have finally gotten around to
it, in 2018.  Thanks to POSIX standards, Stevens' books are as
relevant today as they were back then.

The book being used are this project is his
"Advanced Programming in the UNIX Environment" 3rd edition.
The book has a website with source code, 
[APUE](http://apuebook.com/), but my version has a more
robust GNU Make based build.

The grok/C/APUE project is actually two projects.  One is just working
through the chapters of the APUE book.  The other is more interesting.
I am creating an implementation of Stevens' UNIX System Programming API.
* API implementation: libapue.a and apue.h
* GNU Make build as an initial template for future projects

Currently I only have access to Linux based systems, but I hope to
eventually adapt to and test on other Unix like OS's.

## APUE API & Infrastructure
### [apue.h](include/apue.h)
* Common header file to be included before all other header files
* Builds code conforming to __POSIX.1-2008__ and __XSI 7__ standards
* POSIX is a portmanteau of "Portable Operating System" and "Unix"
* XSI stands for X/OPEN System Interfaces

### libapue.a static library
#### error.c: Error handling routines
* Functions:
  * `err_ret() ` - nonfatal errors for system/library calls (using errno)
  * `err_sys()` - fatal errors for system/library calls (using errno)
  * `err_dump()` - same as above but also dump core
  * `err_cont()` - nonfatal errors for system/library calls (supply error num)
  * `err_exit()` - fatal errors for system/library calls (supply error num)
  * `err_msg() ` - nonfatal errors unrelated to a system/library calls
  * `err_quit()` - fatal errors unrelated to a system/library calls

#### errorlog.c: Error logging routines
* Functions:
  * `log_open()` - initialize syslog
  * `log_ret()` - log nonfatal errors for system/library calls (using errno)
  * `log_sys()` - log fatal errors for system/library calls (using errno)
  * `log_cont()` - log nonfatal errors for system/library calls (supply error num)
  * `log_exit()` - log fatal errors for system/library calls (supply error num)
  * `log_info()` - log messages, unrelated to system/library calls
  * `log_msg()` - log nonfatal errors unrelated to a system/library calls
  * `log_quit()` - log fatal errors unrelated to a system/library calls
*  Callers must globally define and set `log_to_stderr`
  * int `log_to_stderr` = 0: use syslog()
  * int `log_to_stderr` = 1: behave like corresponding `err_*()` vers
* `log_cont()` & `log_info()` not in book

#### limits.c: Determining various systems limits at run time
* Functions:
  * `path_alloc()` - uses malloc to allocate space for pathnames
  * `open_max()` - returns maximum number of possible open file descriptors

### GNU Make based build
* Unlike source code on the book's website, my build is not recursive.
* Individual configuration files are distributed throughout the directory structure.
* Using make include statements to pull everyhing together into one makefile.
* Results in faster, more reliable software builds.
* For vim Syntastic plug-in to work, launch vim from directory with Makefile.

## APUE Book Chapters
* UNIX System Overview - Chapter 1
* UNIX Standardization and Implementation - Chapter 2
* File I/O - Chapter 3
