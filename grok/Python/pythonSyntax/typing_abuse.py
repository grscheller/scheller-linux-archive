#!/usr/bin/env python

from typing import TypeVar, reveal_type

A = TypeVar('A', bound = complex)
B = TypeVar('B', bound = float)

class foo:
    def __init__(self, a: A, b: B) -> None:
        self.a = a
        self.b = b
        self.result: complex = 0

    def compute(self) -> None:
        self.result = self.a + self.b

    def get(self) -> complex:
        if self.result == 0:
            return 42 
        return self.result

f1 = foo(1.0, 1)
ff1 = f1.get()
reveal_type(ff1)
f1.compute()
ff2 = f1.get()
reveal_type(ff2)

bar = (2.1).__add__(3)
b1: int = 1 + 2
reveal_type(b1)
b2: float = b1
reveal_type(b2)

type_b1 = type(b1)
type_b2 = type(b2)
reveal_type(type_b1)
reveal_type(type_b2)

print("type b1: {}".format(type_b1))
print("type b2: {}".format(type_b2))

# But it is always really just an int

at_run_time = """
$ ./typing_abuse.py
Runtime type is 'int'
Runtime type is 'float'
Runtime type is 'int'
Runtime type is 'int'
Runtime type is 'type'
Runtime type is 'type'
type b1: <class 'int'>
type b2: <class 'int'>
"""
