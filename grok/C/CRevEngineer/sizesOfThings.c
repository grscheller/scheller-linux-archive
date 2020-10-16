/*
 *  Purpose to print the size in bytes of different types.
 *  Secondary purpose is to figure out format strings
 *  when given correct types.
 *
 */

#include <stdio.h>
#include <limits.h>

void myFunc1(void);
void myFunc2(void);
void choose_myFunc(void (*func_ptr)(void));

int main(void)
{
    char ch = '1';
    short si = 1;
    int i = 1;
    unsigned int ui = 1;
    long li = 1;
    unsigned long uli = 1;
    long long ll = 1;
    unsigned long long ull = 1;
    float f = 1.0;
    double d = 1.0;
    long double ld = 1.0;

    printf("size of char = %lu bytes\n", sizeof(ch));
    printf("size of short = %lu bytes\n", sizeof(si));
    printf("size of int = %lu bytes\n", sizeof(i));
    printf("size of unsigned int = %lu bytes\n", sizeof(ui));
    printf("size of long = %lu bytes\n", sizeof(li));
    printf("size of unsigned long = %lu bytes\n", sizeof(uli));
    printf("size of long long = %lu bytes\n", sizeof(ll));
    printf("size of unsigned long long = %lu bytes\n", sizeof(ull));
    printf("size of float = %lu bytes\n", sizeof(f));
    printf("size of double = %lu bytes\n", sizeof(d));
    printf("size of long double = %lu bytes\n", sizeof(ld));
    printf("\n");

    int int_max = INT_MAX;
    int int_min = INT_MIN;
    long int_maxLong = INT_MAX;
    long int_minLong = INT_MIN;
    unsigned int uint_max = UINT_MAX;

    printf("INT_MAX as int: %d\n", int_max);
    printf("INT_MIN as int: %d\n", int_min);
    printf("INT_MAX as long: %ld\n", int_maxLong);
    printf("INT_MIN as long: %ld\n", int_minLong);
    printf("UINT_MAX as unsigned long: %u\n", uint_max);
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

    int magic = 42;
    int *p = &magic;

    printf("size of pointer to int = %lu bytes\n", sizeof(p));
    printf("value of pointer to int = %p\n", (void *) p);
    printf("size of int pointed to by pointer = %lu bytes\n", sizeof(*p));
    printf("value of int pointed to by pointer = %d\n", *p);
    printf("\n");
    
    void (*myFunc_ptr)(void);
    int (*main_ptr)(void) = &main;

    myFunc_ptr = &myFunc1;
    choose_myFunc(myFunc_ptr);
    choose_myFunc(myFunc2);
    printf("\n");

    printf("size of pointer to function = %lu bytes\n", sizeof(myFunc_ptr));
    printf("value of pointer to function = %p\n", (void *) myFunc_ptr);
    printf("size of function name \"pointer\" = %lu bytes\n", sizeof(myFunc1));
    printf("value of function name \"pointer\"  = %p\n", (void *) myFunc1);
    printf("size of main \"pointer\" = %lu bytes\n", sizeof(main));
    printf("value of main \"pointer\"  = %p\n", (void *) main);
    printf("difference of function ptrs = %ld\n", (void *)myFunc_ptr - (void *)main_ptr);
    printf("difference of function names = %ld\n", (void *)myFunc1 - (void *)main);

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

