#!/usr/bin/env python

import ctypes

libc = ctypes.CDLL('/lib64/libc.so.6')
t = libc.time(None)
print(t)
