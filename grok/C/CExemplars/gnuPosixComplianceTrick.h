/*
 *  Trick used in /usr/include/bits/confname.h
 *  by the GNU glibc header files to define
 *  the _SC_*, _PC_*, and _SC_* macros used
 *  with the sysconf, pathconf, and confstr
 *  library functions.
 *
 *  The POSIX standard calls for _PC_*, _SC_*, and _SC_*
 *  to be defined as precompiler macros.  GNU Linux
 *  implements them as enumerations, but then turns around
 *  and defines the macros to be the enumeration symbols.
 *
 *  Kinda silly, the sole purpose of these macros
 *  are just to meet the letter of the POSIX standard.
 *
 *  In my own code I would have made the enumeration a
 *  different symbol, perhaps lowercase.  I understand
 *  GNU not wanting to namespace pollute.
 *
 *  A comment should have been added to the header file
 *  explaining what was being done.  Better yet, POSIX
 *  should be amended to allow for this feature to be
 *  implemented as enumerations.
 *
 *  I hate having to simultaneously program in more than
 *  one language at a time.  The less one has to use
 *  thr precompiler, the better.  Hopefully real modules
 *  will be coming to C sometime soon, its beem 40 years 
 *  too late.
 */
enum {
    foo0,
# define foo0 foo0
    foo1,
# define foo1 foo1
    foo2,
# define foo2 foo2
    foo3,
# define FOO3 foo3
    foo4
};
