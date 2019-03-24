## Advanced Programming in the UNIX Environment (APUE)
The goal of the grok/C/APUE project is to write portible POSIX
compliant systems oriented C language code.  This may be becoming
a bit old fashioned and against the current trend to program
for the "big" three: Linux/MacOS/Windows using either GCC/GLIBC
or Clang/LLVM.

The book being used are this project is
"Advanced Programming in the UNIX Environment" 3rd edition,
by W. Richard Stevens.  The book has a website with source code, 
[APUE](http://apuebook.com/)

I have always admired W. Richard Stevens' books on Unix System
programming.  Back in the mid 1990's, I thought someday I would
work my way through them.  Well, I have finally gotten around to
it, in 2018.  Thanks to maturity of the POSIX standards, Stevens'
books are still very relevant today.

## APUE project includes:
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
### [apue.h](include/apue.h) header
* common header file to be included before all other header files
* builds code conforming to __POSIX.1-2008__ and __XSI 7__ standards
* POSIX is a portmanteau of "Portable Operating System" and "UNIX"
* XSI stands for X/OPEN System Interfaces

### [libapue.a](src/libapue) static library
#### Error handling routines: error.c 
* functions:
  * `err_ret()`  - nonfatal errors for system/library calls (using errno)
  * `err_sys()`  - fatal errors for system/library calls (using errno)
  * `err_dump()` - same as above but also dump core
  * `err_cont()` - nonfatal errors for system/library calls (supply error num)
  * `err_exit()` - fatal errors for system/library calls (supply error num)
  * `err_msg()`  - nonfatal errors unrelated to a system/library calls
  * `err_quit()` - fatal errors unrelated to a system/library calls

#### Error logging routines: errorlog.c 
* functions:
  * `log_open()` - initialize syslog
  * `log_ret()`  - log nonfatal errors for system/library calls (using errno)
  * `log_sys()`  - log fatal errors for system/library calls (using errno)
  * `log_cont()` - log nonfatal errors for system/library calls (supply error num)
  * `log_exit()` - log fatal errors for system/library calls (supply error num)
  * `log_info()` - log messages, unrelated to system/library calls
  * `log_msg()`  - log nonfatal errors unrelated to a system/library calls
  * `log_quit()` - log fatal errors unrelated to a system/library calls
* callers must globally define and set `log_to_stderr`
  * `log_to_stderr` = 0: use syslog()
  * `log_to_stderr` = 1: behave like corresponding `err_*()` versions
* `log_cont()` & `log_info()` not in book

#### Determining systems limits at run time: limits.c 
* functions:
  * `path_alloc()` - uses malloc to allocate space for pathnames
  * `open_max()` - returns maximum number of possible open file descriptors

### GNU Make based build
* unlike source code on the book's website, my build is not recursive.
* individual configuration files distributed throughout directory structure.
* using make include statements to pull everyhing together into one makefile.
* results in faster, more reliable software builds.
* for vim Syntastic plug-in to work, launch vim from directory with Makefile.

## APUE Book Chapters
* UNIX System Overview - Chapter 1
* UNIX Standardization and Implementation - Chapter 2
* File I/O - Chapter 3
* Files and Directories - Chapter 4
