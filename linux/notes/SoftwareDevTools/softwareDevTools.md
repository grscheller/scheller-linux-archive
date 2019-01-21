# Software Development Tools
Command line tools useful in software development, especially
C and C++ software development.

See the system manpages for detailed descriptions of these commands.
## cc
Compile C language programs,
### Partially building a C program
* gcc -E  --> preprocessor, but don't compile
* gcc -S  --> compile but don't assemble
* gcc -c  --> assemble but don't link
* gcc     --> build an executable

## ar
Create, modify, and extract items from archives.  Usually what is
created or modified is a static shared library of `*.o` object files.
### ar rcs
Create/update an archive static library and symbol table.
```
    $ ar rcs libapue2.a errorHandlers.o limits.o
```
## nm 
List symbols from object files.
### nm
With no arguments, nm lists the symbols from object files
and ar archives of object files.

For object files:
```
    $ nm errorHandlers.o
                     U abort
    0000000000000000 T err_cont
    0000000000000580 t err_doit
    00000000000000d1 T err_dump
    000000000000018f T err_exit
    0000000000000250 T err_msg
                     U __errno_location
    0000000000000325 T err_quit
    00000000000003ea T err_ret
    00000000000004bd T err_sys
                     U exit
                     U fflush
                     U fputs
                     U _GLOBAL_OFFSET_TABLE_
                     U snprintf
                     U __stack_chk_fail
                     U stderr
                     U stdout
                     U strerror
                     U strlen
                     U vsnprintf
```
For ar archives:
```
    $ nm libapue2.a 

    errorHandlers.o:
                     U abort
    0000000000000000 T err_cont
    0000000000000580 t err_doit
    00000000000000d1 T err_dump
    000000000000018f T err_exit
    0000000000000250 T err_msg
                     U __errno_location
    0000000000000325 T err_quit
    00000000000003ea T err_ret
    00000000000004bd T err_sys
                     U exit
                     U fflush
                     U fputs
                     U _GLOBAL_OFFSET_TABLE_
                     U snprintf
                     U __stack_chk_fail
                     U stderr
                     U stdout
                     U strerror
                     U strlen
                     U vsnprintf

    limits.o:
                     U __errno_location
                     U err_sys
                     U _GLOBAL_OFFSET_TABLE_
                     U malloc
    0000000000000142 T open_max
    0000000000000010 b openmax
    0000000000000000 T path_alloc
                     U pathconf
    0000000000000000 d pathmax
    0000000000000000 b posix_version
                     U sysconf
    0000000000000008 b xsi_version
```
### nm -s
Before listing the symbols from an archive, print the index for
which modules contain which names, if one exists.

The mapping is created via either the `ranlib` or `ar s` commands. 
```
    $ nm -s libapue2.a 

    Archive index:
    err_cont in errorHandlers.o
    err_dump in errorHandlers.o
    err_exit in errorHandlers.o
    err_msg in errorHandlers.o
    err_quit in errorHandlers.o
    err_ret in errorHandlers.o
    err_sys in errorHandlers.o
    path_alloc in limits.o
    open_max in limits.o

    errorHandlers.o:
                     U abort
    0000000000000000 T err_cont
    0000000000000580 t err_doit
    00000000000000d1 T err_dump
    000000000000018f T err_exit
      .
      .
      .
```
