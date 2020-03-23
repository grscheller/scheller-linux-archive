"""Library of functions of a pure mathematical nature.
   Efficient, robust design, type checking the
   responsibility of the calling function."""

import sys

__all__ = ['gcd', 'lcm',
           'fibonacci', 'fibonacci_array', 'fibonacci_tuple',
           'fibonacci_mult', 'fibonacci_mult_array', 'fibonacci_mult_tuple',
           'prime_array', 'pythag3', 'ackermann']

## Number Theory mathematical Functions.

def gcd(n, m):
    """Uses euclidian algorithm to compute the gcd of two integers.
       Takes two integers, returns gcd >= 0.

       Note: gcd(0,0) returns 0 while in this case the gcd does not exist."""

    while m > 0:
        n, m = m, n % m

    return n


def lcm(n, m):
    """Finds the least common multiple of two integers.
       Takes two integers, returns lcm >=0."""

    common = gcd(n, m)
    n //= common
    m //= common

    return abs(n*m*common)


def prime_array(n=2, m=100):
    """Return an array of primes where n <= prime < m, uses
       the Sieve of Eratosthenes algorithm

       Usage: primes = prime_array(n,m)"""

    n = abs(n)
    m = abs(m)

    sieve = [x for x in range(3, m, 2) if x % 3 != 0]
    stop = int(m**(0.5)) + 1
    front = -1
    for prime in sieve:
        front += 1
        if prime > stop:
            break
        for pot_prime in sieve[-1:front:-1]:
            if pot_prime % prime == 0:
                sieve.remove(pot_prime)

    if n <= 3 < m:           # We missed [2, 3] but saved
        sieve.insert(0, 3)   # about 60% of initial storage.
    if n <= 2 < m:
        sieve.insert(0, 2)

    sieve = [x for x in sieve if x >= n]  # trim unwanted values

    return sieve


## Fibonacci related mathematical functions.

def fibonacci(fib0=0, fib1=1, count=1000):
    """Returns an iterator to a Fibonacci sequence whose
       first two terms are f0 and f1.  The iterator ends
       after count times.

       Please note: fib0 and fib1 can be any objects where
       the "+" operator has been defined."""

    n = 0
    while n < count:
        n = n + 1
        yield fib0
        fib0, fib1 = fib1, fib0+fib1


def fibonacci_mult(fib0=0, fib1=1, count=1000):
    """Returns an iterator to a Fibonacci sequence using
       * instead of +.  The first two terms are f0 and f1.
       The iterator ends after count times.

       Please note: f0 and f1 can be any objects where
       the "*" operator has been defined."""

    n = 0
    while n < count:
        n = n + 1
        yield fib0
        fib0, fib1 = fib1, fib0*fib1


def fibonacci_array(fib0=0, fib1=1, count=1000):
    """Returns an array with a fibonacci sequence."""

    return [fibonacci(fib0, fib1, count)]


def fibonacci_tuple(fib0=0, fib1=1, count=1000):
    """Returns a tuple with a fibonacci sequence."""

    return tuple(fibonacci_array(fib0, fib1, count))


def fibonacci_mult_array(fib0=0, fib1=1, count=1000):
    """Returns an array with a fibonacci sequence using * instead of +."""

    return [fibonacci_mult(fib0, fib1, count)]


def fibonacci_mult_tuple(fib0=0, fib1=1, count=1000):
    """Returns a tuple with a fibonacci sequence using * instead of +."""

    return tuple(fibonacci_array(fib0, fib1, count))


## Pythagorian Triples related mathematical functions.

def pythag3(a_max=3, all_max=0):
    """This iterator finds all primative pythagorian triples
       up to a given level.  A pythagorian triple are three
       integers (a,b,c) such that a^2 + b^2 = c^2 where
       x,y,z > 0 and gcd(a,b,c) = 1

       If called with one argument generates all triples with
       a <= a_max

       If called with two arguments generate all triples with
       a <= a_max and a,b,c <= all_max"""

    if all_max == 0:
        # No more triples beyond this.
        b_max = lambda a: (a**2 - 1)//2
    else:
        # cap it
        if all_max < a_max + 2:
            a_max = all_max - 2
        b_max = lambda a: min((a**2 - 1)//2, int((all_max**2 - a**2)**0.5))
    c_max = int((a_max**2 + b_max(a_max)**2)**(0.5))

    squares = {}
    for hypotenuse in range(5, c_max + 1, 2):    # hypotenuse always odd
        squares[hypotenuse**2] = hypotenuse

    for a in range(3, a_max + 1):
        for b in range(a + 1, b_max(a) + 1):
            csq = a**2 + b**2
            if csq in squares:
                if gcd(a, b) == 1:
                    yield (a, b, squares[csq])


def ackermann(m=0, n=0):
    """ackermann function is defined recursively by:
        ackermann(0,n) = n+1
        ackermann(m,0) = ackermann(m-1,1)
        ackermann(m,n) = ackermann(m-1, ackermann(m, n-1)) for n,m > 0"""

    # Model a function stack with an array, then
    # evaluate innermost ackermann function first.
    acker = [m, n]

    while len(acker) > 1:
        [m, n] = acker[-2:]
        if m < 1:
            acker[-1] = acker.pop() + 1
        elif n < 1:
            acker[-2] = acker[-2] - 1
            acker[-1] = 1
        else:
            acker[-2] = m - 1
            acker[-1] = m
            acker.append(n-1)

    return acker[0]


if __name__ == '__main__':
    sys.exit(0)
