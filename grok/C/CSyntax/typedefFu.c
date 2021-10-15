/*
 * A typedef replaces a complicated declaration 
 * with a simplier expression.
 *
 * The  trick is to know that the typedef you
 * are defining goes in the corresponding location
 * that a variable would go for the declaration
 * defined if you just drop the word typedef.
 *
 */
#include <stdio.h>

typedef int BinOp(int, int);
typedef int TriOp(int, int, int);

BinOp add;
BinOp mult;
TriOp mult_a_sum;

int
main(void)
{
    // trivial example
    //   Just another name for an integer.
    typedef int myint;

    myint i = 7;
    myint j = 13;
    myint k = 42;
    myint ww = mult_a_sum(i, j ,k);

    // more complicated example
    //   An array of 3 pointers to integers
    typedef int* Foo[3];
    
    Foo foo = { &i, &j, &k };
    printf("%d + %d = %d\n", *foo[0], *foo[2], *foo[0] + *foo[2]);

    // repeat without typedef
    int* bar[3] = { &j, &k, &i };
    printf("%d + %d = %d\n", *bar[0], *bar[2], *bar[0] + *bar[2]);

    // forward reference with typedefs
    printf("%d*(%d + %d) = %d\n", k, i, j, mult(k, add(i,j)));
    printf("%d*(%d + %d) = %d\n", i, j, k, mult_a_sum(i,j,k));
    printf("ww = %d\n", ww);

    return 0;
}

int
add(int x, int y)
{
    return x + y;
}

int
mult(int w, int z)
{
    return w * z;
}

int
mult_a_sum(int i, int j, int k)
{
    return mult(i, add(j, k));
}
