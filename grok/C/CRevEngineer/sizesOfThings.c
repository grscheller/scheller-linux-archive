/*
 *  Purpose to print the size in bytes of different types.
 *
 *  Secondary purpose is to figure out format strings to use
 *  for the C99 standard.  Note: [parameter] is POSIX extension.
 *
 *  Syntax Format Placeholder:
 *    %[parameter][flags][width][.precision][length]type
 *
 *  parameter:
 *    <n>$ where <n> is positional parameter
 *        example: printf("%2$d %1$#o %2$#x", 16, 17)
 *        outputs: 17 020 0x11
 *
 *  flags:
 *    - left-align output, default is to right-align
 *    + prepend plus sign for positive signed-numeric types
 *      (space) prepends space for positive signed types, ignored if + flag given
 *    0 prepend zeros for numeric types when 'width' option given, default is spaces.
 *        example: printf("[%4X]",3) produces [   3]
 *        example: printf("[%04X]",3) produces [0003]
 *    ' thousands grouping separator applied to integer or exponent of a decimal 
 *    # alternate form:
 *        for g and G types, trailing zeros are not removed
 *        for f, F, e, E, g, G types, output always contains a decimal point
 *        for o, x, X types,aprepend 0, 0x, 0X, respectively, to non-zero numbers
 *
 *  width and precision:
 *    minimum and maximum output - depends on type
 *        example: printf("[%10s]", "abc")       gives [       abc]
 *        example: printf("[%-10.3s]", "abcdef") gives [abc       ]
 *        example: printf("[%*d]", 10, 42)       gives [        42]
 *        example: printf("[%10.3f]", 3.14159)   gives [     3.142]
 *        example: printf("[%10.3g]", 3.14159)   gives [      3.14]
 *
 *  length:
 *    Integer Types:
 *      hh expect int argument promoted from char
 *      h  expect int argument promoted from short
 *      l  expect long integer argument
 *      ll expect long long integer argument
 *      z  expect a size_t integer argument
 *      j  expect a intmax_t integer argument
 *      t  expect a ptrdiff_t integer argument
 *
 *    Floating Point Types:
 *      l  ignored, float arguments always promoted to double in varargs calls
 *      L  expect a long double argument
 *
 *  type:
 *    %	    print a literal % character, doesn't accept flags, width, precision, length fields
 *    d, i  int as a signed integer. %d and %i are same printf, but are different for scanf
 *          for input %i will interpret number as hexadecimal or octal if preceded by 0x or 0 respectfully
 *    u     print decimal unsigned int
 *    f, F  double in normal (fixed-point) notation. f and F differ only in capitalization
 *            nan vs NAN, infinity vs INFINITY, etc
 *    e, E  double value in exponential form d.ddde±dd or d.dddE±dd, exponent always contains at least two digits
 *    g, G  double in either normal or exponential notation, usually gives what you want
 *    x, X  unsigned int as a hexadecimal number. x uses lower-case and X uses upper-case letters
 *    o     unsigned int in octal
 *    s     null-terminated string
 *    c     char (character)
 *    p     void* (pointer to void) in an implementation-defined format
 *    a, A  double in hexadecimal notation, starting with 0x or 0X
 *    n     print nothing, but writes the number of characters successfully written so far into an integer pointer parameter.
 */

#include <stdio.h>
#include <limits.h>   /* for macros like INT_MAX, LONG_MAX, etc */
#include <stddef.h>   /* for wchar_t, ptrdiff_t */
#include <stdint.h>   /* for intmax_t */

void myFunc1(void);
void myFunc2(void);
void choose_myFunc(void (*func_ptr)(void));

int main(void)
{
    char ch = '1';
    short si = 1;
    int i = 1;
    unsigned int ui = 1;
    unsigned u = 1;
    long li = 1;
    unsigned long uli = 1;
    long long ll = 1;
    unsigned long long ull = 1;
    float f = 1.0;
    double d = 1.0;
    long double ld = 1.0;
    intmax_t imax = 1;

    printf("sizeof char = %zu bytes\n", sizeof ch);
    printf("sizeof short = %zu bytes\n", sizeof si);
    printf("sizeof int = %zu bytes\n", sizeof i);
    printf("sizeof unsigned = %zu bytes\n", sizeof u);
    printf("sizeof unsigned int = %zu bytes\n", sizeof ui);
    printf("sizeof long = %zu bytes\n", sizeof li);
    printf("sizeof unsigned long = %zu bytes\n", sizeof uli);
    printf("sizeof long long = %zu bytes\n", sizeof ll);
    printf("sizeof unsigned long long = %zu bytes\n", sizeof ull);
    printf("sizeof intmax_t = %zu bytes\n", sizeof imax);
    printf("sizeof float = %zu bytes\n", sizeof f);
    printf("sizeof double = %zu bytes\n", sizeof d);
    printf("sizeof long double = %zu bytes\n", sizeof ld);
    printf("\n");

    printf("sizeof char = %zu bytes\n", sizeof (char));
    printf("sizeof short = %zu bytes\n", sizeof (short));
    printf("sizeof (int) = %zu bytes\n", sizeof (int));
    printf("sizeof (unsigned) = %zu bytes\n", sizeof (unsigned));
    printf("sizeof (unsigned int) = %zu bytes\n", sizeof (unsigned int));
    printf("sizeof (long) = %zu bytes\n", sizeof (long));
    printf("sizeof (unsigned long) = %zu bytes\n", sizeof (unsigned long));
    printf("sizeof (long long) = %zu bytes\n", sizeof (long long));
    printf("sizeof (unsigned long long) = %zu bytes\n", sizeof (long long));
    printf("sizeof (intmax_t) = %zu bytes\n", sizeof (intmax_t));
    printf("sizeof float = %zu bytes\n", sizeof (float));
    printf("sizeof double = %zu bytes\n", sizeof (double));
    printf("sizeof long double = %zu bytes\n", sizeof (long double));
    printf("\n");

    printf("sizeof (void) = %zu bytes\n", sizeof (void));
    printf("sizeof (void*) = %zu bytes\n", sizeof (void*));
    printf("sizeof (size_t) = %zu bytes\n", sizeof (size_t));
    printf("sizeof (intmax_t) = %zu bytes\n", sizeof (intmax_t));
    printf("sizeof (wchar_t) = %zu bytes\n", sizeof (wchar_t));
    printf("sizeof (ptrdiff_t) = %zd bytes\n", sizeof (ptrdiff_t));
    printf("\n");

    int int_max = INT_MAX;
    int int_min = INT_MIN;
    long int_maxLong = INT_MAX;
    long int_minLong = INT_MIN;
    unsigned int uint_max = UINT_MAX;

    printf("INT_MAX as int: %d\n", int_max);
    printf("INT_MIN as int: %d\n", int_min);
    printf("UINT_MAX as unsigned int: %u\n", uint_max);
    printf("INT_MAX as long: %ld\n", int_maxLong);
    printf("INT_MIN as long: %ld\n", int_minLong);
    printf("UINT_MAX as long: %ld\n", (long) uint_max);
    printf("\n");

    long long_max = LONG_MAX;
    long long_min = LONG_MIN;
    unsigned long ulong_max = ULONG_MAX;

    printf("LONG_MAX as long: %ld\n", long_max);
    printf("LONG_MIN as long: %ld\n", long_min);
    printf("LONG_MAX as long formatted unsigned: %lu\n", long_max);
    printf("LONG_MIN as long formatted unsigned: %lu\n", long_min);
    printf("ULONG_MAX as unsigned long: %lu\n", ulong_max);
    printf("\n");

    long long llong_max = LLONG_MAX;
    long long llong_min = LLONG_MIN;
    unsigned long long ullong_max = ULLONG_MAX;

    printf("LLONG_MAX as long long: %lld\n", llong_max);
    printf("LLONG_MIN as long long: %lld\n", llong_min);
    printf("ULLONG_MAX as unsigned long long: %llu\n", ullong_max);
    printf("\n");

    intmax_t intmax_max = INTMAX_MAX;
    intmax_t intmax_min = INTMAX_MIN;
    uintmax_t uintmax_max = UINTMAX_MAX;

    printf("INTMAX_MAX as intmax_t: %jd\n", intmax_max);
    printf("INTMAX_MIN as intmax_t: %jd\n", intmax_min);
    printf("UINTMAX_MAX as unsigned intmax_t: %ju\n", uintmax_max);
    printf("\n");

    int magic = 42;
    int *p = &magic;

    printf("size of pointer to int = %zu bytes\n", sizeof(p));
    printf("value of pointer to int = %p\n", (void *) p);
    printf("value of pointer to int = %p\n", p);
    printf("size of int pointed to by pointer = %zu bytes\n", sizeof(*p));
    printf("value of int pointed to by pointer = %d\n", *p);
    printf("\n");
    
    void (*myFunc_ptr)(void);
    void (*anotherFunc_ptr)(void);
    int (*main_ptr)(void) = &main;

    myFunc_ptr = &myFunc1;
    anotherFunc_ptr = &myFunc2;
    choose_myFunc(myFunc_ptr);
    choose_myFunc(myFunc2);
    printf("\n");

    printf("size of pointer to function = %zu bytes\n", sizeof(myFunc_ptr));
    printf("value of pointer to function = %p\n", (void *) myFunc_ptr);
    printf("size of function name \"pointer\" = %zu bytes\n", sizeof(myFunc1));
    printf("value of function name \"pointer\"  = %p\n", (void *) myFunc1);
    printf("size of main \"pointer\" = %zu bytes\n", sizeof(main));
    printf("value of main \"pointer\"  = %p\n", (void *) main);
    printf("difference of function ptrs (same type) = %td\n", myFunc_ptr - anotherFunc_ptr);
    printf("difference of function names (same type) = %td\n", myFunc1 - myFunc2);
    printf("difference of function ptrs (different types) = %td\n", (void *)myFunc_ptr - (void *)main_ptr);
    printf("difference of function names (different types) = %td\n", (void *)myFunc1 - (void *)main);

    return 0;
}

void myFunc1(void)
{
    printf("entering myFunc1\n");
    printf("exiting myFunc1\n");
}

void myFunc2(void)
{
    printf("entering myFunc2\n");
    printf("exiting myFunc2\n");
}

void choose_myFunc(void (*func_ptr)(void))
{
    printf("entering choose_myFunc\n");
    (*func_ptr)();
    printf("exiting choose_myFunc\n");
}

