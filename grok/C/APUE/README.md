# Advanced Programming in the UNIX Environment (APUE)

The goal of ths project is to write portable POSIX compliant
systems oriented C language code.  This may be becoming
a bit old fashioned and against the current trend to program
for the "big" three: Linux/MacOS/Windows using either GCC/GLIBC
or Clang/LLVM.

The book being used for this project is
"Advanced Programming in the UNIX Environment" 3rd edition,
by W. Richard Stevens.  The book has a website with source code,
[APUE](http://apuebook.com/)

I have always admired W. Richard Stevens' books on Unix System
programming.  Back in the mid 1990's, I thought someday I would
work my way through them.  Well, I have finally gotten around to
it, in 2018.  Thanks to maturity of the POSIX standards, Stevens'
books are still very relevant today.

## APUE project includes

* implementation of Steven's APUE UNIX System Programming API
* robust non-recursive GNU Make build
* working through the chapters in Steven's book
* exploring the evolution and direction ot Feature Test Macros
* Test against
  * Arch Linux (GCC/GLIBC)
  * Arch Linux (Clang/LLVM)
  * CentOS 6 and 7 (GCC/GLIBC)
  * Cygwin (Windows 10 64-bit)
  * MSYS2 (Windows 10 64-bit)
  * FreeBSD
  * MacOS

## APUE API & Infrastructure

### [config.mk](config.mk)

* manually editted by end user for target system
* program to __ISO/IEC 9899:1999__ C99 standard

### [apue.h](src/apue/apue.h) header

* common header file to be included before all other header files
* builds code conforming to __POSIX.1-2008__ and __XSI 7__ standards
* POSIX is a portmanteau of "Portable Operating System" and "UNIX"
* XSI stands for X/OPEN System Interfaces

### [libapue.a](src/apue) static library

#### Error handling routines: `error.c

* functions:
  * `void err_ret(const char*, ...)`:
    * nonfatal errors for system/library calls (using errno)
  * `void err_sys(const char*, ...)`:
    * fatal errors for system/library calls (using errno)
  * `void err_dump(const char*, ...)`:
    * same as above but also dump core
  * `void err_cont(int, const char*, ...)`:
    * nonfatal errors for system/library calls (supply error num)
  * `void err_exit(int, const char*, ...)`:
    * fatal errors for system/library calls (supply error num)
  * `void err_msg(const char*, ...)`:
    * nonfatal errors unrelated to a system/library calls
  * `void err_quit(const char*, ...)`:
    * fatal errors unrelated to a system/library calls

#### Error logging routines: `errorlog.c

* functions:
  * `void log_open(const char *ident, int option, int facility)`:
    * initialize syslog
  * `void log_ret(const char*, ...)`:
    * log nonfatal errors for system/library calls (using errno)
  * `void log_sys(const char*, ...)`:
    * log fatal errors for system/library calls (using errno)
  * `void log_cont(int, const char*, ...)`:
    * log nonfatal errors for system/library calls (supply error num)
  * `void log_exit(int, const char*, ...)`:
    * log fatal errors for system/library calls (supply error num)
  * `void log_info(const char*, ...)`:
    * log messages, unrelated to system/library calls
  * `void log_msg(const char*, ...)`:
    * log nonfatal errors unrelated to a system/library calls
  * `void log_quit(const char*, ...)`:
    * log fatal errors unrelated to a system/library calls
* callers must globally define and set `log_to_stderr`
  * `int log_to_stderr = 0`: use syslog()
  * `int log_to_stderr = 1`: behave like corresponding `err_*()` versions
* `log_cont()` & `log_info()`not in book

#### Determining systems limits at run time: `limits.c`

* functions:
  * `char* path_alloc(size_t *)` - uses malloc to allocate space for pathnames
  * `long open_max(void)` - returns maximum number of possible open file descriptors

#### Format a message and allocate sufficient space for it: `make_message.c`

* functions:
  * `char* make_message(const char *fmt, ...)`

### GNU Make based build

* unlike source code on the book's website, my build is not recursive
* individual configuration files distributed throughout directory structure
* using make include statements to pull everyhing together into one makefile
* results in faster, more reliable software builds
* for vim Syntastic plug-in to work, launch vim from directory with Makefile

## APUE Book Chapters

* UNIX System Overview - Chapter 1
* UNIX Standardization and Implementation - Chapter 2
* File I/O - Chapter 3
* Files and Directories - Chapter 4
