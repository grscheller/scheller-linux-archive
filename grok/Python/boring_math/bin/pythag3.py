#!/usr/bin/env pypy3
# Copyright 2016-2023 Geoffrey R. Scheller
#
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
