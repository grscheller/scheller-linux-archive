#!/usr/bin/env python
"""Find all primative pythagorian triples up to a given level.

A pythagorian triple are three integers (a,b,c) such
that a^2 + b^2 = c^2 where x,y,z > 0 and gcd(a,b,c) = 1

Usage: pythag3.py n [m]

If called with one argument generate all triples with a <= n.
If called with two arguments generate all triples with
a <= n and a,b,c <= m

For pythag3.py to find pureMath library: export PYTHONPATH=../lib
"""

__author__ = "Geoffrey Scheller"

import sys
from integer_math import pythag3

def main():
    """Main entry point for pythag3.py"""

    # Argument processing with some idiot checking
    args = sys.argv[1:]

    if len(args) == 1:
        pythag3_iter = pythag3(int(args[0]))
    elif len(args) == 2:
        pythag3_iter = pythag3(int(args[0]), int(args[1]))
    else:
        pythag3_iter = pythag3()

    # Print out Pythagean Triples
    for triple in pythag3_iter:
        print(triple)

if __name__ == "__main__":
    main()
