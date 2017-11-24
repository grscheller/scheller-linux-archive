"""Library of functions of a pure mathematical nature.
   Efficient, robust design, no idiot checking."""

# Try to make both Python 2.7/3.3 compatible
from __future__ import (absolute_import, division,
                        print_function, unicode_literals)

import sys
PYTHON2 = (sys.version_info[0] == 2)
if PYTHON2:
    range = xrange

__all__ = ['gcd', 'lcm',
           'fib', 'fibArray', 'fibTuple',
           'fibMult', 'fibMultArray', 'fibMultTuple',
           'primeArray', 'pythag3', 'ackermann']


## Number Theory mathematical Functions.

def gcd(n, m):
    """Uses euclidian algorithm to compute the gcd of two integers.
       Takes two integers, returns gcd >= 0.

       Note: gcd(0,0) returns 0 while in this case the gcd does not exist."""

    n = abs(int(n))
    m = abs(int(m))

    while m > 0:
        n, m = m, n % m

    return n


def lcm(n, m):
    """Finds the least common multiple of two integers.
       Takes two integers, returns lcm >=0."""

    common = gcd(n, m)
    n1 = abs(int(n))//common
    m1 = abs(int(m))//common

    return n1*m1*common


def primeArray(n=2, m=100):
    """Return an array of primes where n <= prime < m, uses
       the Sieve of Eratosthenes algorithm

       Usage: primes = primeArray(n,m)"""

    n = abs(int(n))
    m = abs(int(m))

    sieve = [x for x in range(3, m, 2) if x % 3 != 0]
    stop = int(m**(0.5)) + 1
    front = -1
    for x in sieve:
        front += 1
        if x > stop:
            break
        for y in sieve[-1:front:-1]:
            if y % x == 0:
                sieve.remove(y)

    if n <= 3 and m > 3:     # We missed [2, 3] but saved
        sieve.insert(0, 3)   # about 60% of initial storage.
    if n <= 2 and m > 2:
        sieve.insert(0, 2)

    sieve = [x for x in sieve if x >= n]  # trim unwanted values

    return sieve


## Fibonacci related mathematical functions.

def fib(f0=0, f1=1, count=1000):
    """Returns an iterator to a Fibonacci sequence whose
       first two terms are f0 and f1.  The iterator ends
       after count times.

       Please note: f0 and f1 can be any objects where
       the "+" operator has been defined."""

    n = 0
    while n < count:
        n = n + 1
        yield f0
        f0, f1 = f1, f0+f1


def fibMult(f0=0, f1=1, count=1000):
    """Returns an iterator to a Fibonacci sequence using
       * instead of +.  The first two terms are f0 and f1.
       The iterator ends after count times.

       Please note: f0 and f1 can be any objects where
       the "*" operator has been defined."""

    n = 0
    while n < count:
        n = n + 1
        yield f0
        f0, f1 = f1, f0*f1


def fibArray(f0=0, f1=1, count=1000):
    """Returns an array with a fibonacci sequence."""

    return [f for f in fib(f0, f1, count)]


def fibTuple(f0=0, f1=1, count=1000):
    """Returns a tuple with a fibonacci sequence."""

    return tuple(fibArray(f0, f1, count))


def fibMultArray(f0=0, f1=1, count=1000):
    """Returns an array with a fibonacci sequence using * instead of +."""

    return [f for f in fibMult(f0, f1, count)]


def fibMultTuple(f0=0, f1=1, count=1000):
    """Returns a tuple with a fibonacci sequence using * instead of +."""

    return tuple(fibMultArray(f0, f1, count))


## Pythagorian Triples related mathematical functions.

def pythag3(amax=3, max=0):
    """This iterator finds all primative pythagorian triples
       up to a given level.  A pythagorian triple are three
       integers (a,b,c) such that a^2 + b^2 = c^2 where
       x,y,z > 0 and gcd(a,b,c) = 1

       If called with one argument this program generates
       all triples with a <= amax.
       If called with two arguments it then generates all
       triples where a <= amax and a,b,c <= max"""

    if max == 0:
        # No more triples beyond this.
        bmax = lambda a: (a**2 - 1)//2
    else:
        # cap it
        if max < amax + 2:
            amax = max - 2
        bmax = lambda a: min((a**2 - 1)//2, int((max**2 - a**2)**0.5))
    cmax = int((amax**2 + bmax(amax)**2)**(0.5))

    squares = {}
    for jj in range(5, cmax+1, 2):    # hypotenuse always odd
        squares[jj**2] = jj

    for a in range(3, amax+1):
        for b in range(a+1, bmax(a)+1):
            csq = a**2 + b**2
            if csq in squares:
                if gcd(a, b) == 1:
                    yield (a, b, squares[csq])


def ackermann(m=0, n=0):
    """ackermann function is defined recursively by:
        ackermann(0,n) = n+1
        ackermann(m,0) = ackermann(m-1,1)
        ackermann(m,n) = ackermann(m-1, ackermann(m, n-1)) for n,m > 0"""

    b = [m, n]

    while len(b) > 1:
        [mm, nn] = b[-2:]
        if mm < 1:
            b[-1] = b.pop() + 1
        elif nn < 1:
            b[-2] = b[-2] - 1
            b[-1] = 1
        else:
            b[-2] = mm - 1
            b[-1] = mm
            b.append(nn-1)

    return b[0]


if __name__ == '__main__':
    exit(0)
