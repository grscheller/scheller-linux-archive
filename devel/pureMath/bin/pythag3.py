#!/usr/bin/env python
"""This script finds all primative pythagorian triples up to a given
   level.  A pythagorian triple are three integers (a,b,c) such
   that a^2 + b^2 = c^2 where x,y,z > 0 and gcd(a,b,c) = 1

   Usage: pythag3a.py n [m]

   If called with one argument this program
         generates all triples with  a <= n.
   If called with two arguments it then generates
         all triples where a <= n and a,b,c <= m"""

# Try to make both Python 2.7/3.3 compatible
from __future__ import (absolute_import, division,
                        print_function, unicode_literals)
import sys
PYTHON2 = (sys.version_info[0] == 2)
if PYTHON2:
    range = xrange

import pureMath as pm

# Argument processing with idiot checking:
args = sys.argv[1:]
if len(args) == 1:
    pythag3_iter = pm.pythag3(int(args[0]))
elif len(args) == 2:
    pythag3_iter = pm.pythag3(int(args[0]), int(args[1]))
else:
    pythag3_iter = pm.pythag3()

for triple in pythag3_iter:
    print(triple)
