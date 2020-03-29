#!/usr/bin/env python
"""Compute the Ackermann function.

Ackermann function is defined recursively by:

ackermann(0,n) = n+1
ackermann(m,0) = ackermann(m-1,1)
ackermann(m,n) = ackermann(m-1, ackermann(m, n-1)) for n,m > 0

Usage: ackermann.py m n
"""

__author__ = "Geoffrey Scheller"

import sys
from integer_math import ackermann

def main():
    """Main entry point for ackerman.py"""

    # Argument parsing and checking
    args = sys.argv[1:]
    if len(args) == 2:
        try:
            m = int(args[0])
            n = int(args[1])
            if m < 0 or n < 0:
                print("Error: Negative integer argument given"
                      , file=sys.stderr)
                sys.exit(1)
        except ValueError:
            print("Error: Non-integer argument given", file=sys.stderr)
            sys.exit(1)
    else:
        print("Error: ackermann.py takes 2 arguments", file=sys.stderr)
        sys.exit(1)

    # Compute value
    print(ackermann(m, n))

if __name__ == "__main__":
    main()
