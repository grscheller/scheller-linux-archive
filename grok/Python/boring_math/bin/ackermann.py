#!/usr/bin/env pypy3
# Copyright 2016-2023 Geoffrey R. Scheller
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
            m_arg = int(args[0])
            n_arg = int(args[1])
            if m_arg < 0 or n_arg < 0:
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
    print(ackermann(m_arg, n_arg))


if __name__ == "__main__":
    main()
