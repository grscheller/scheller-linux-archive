#!/usr/bin/env python

import sys

"""Test scoping rules to see how an inner scope can affect an outer scope.

Python will try and stop you from doing this stupid thing, but you can still
do it you use a mutable container for the value you want to change. 
"""

foo = "Foofoo"
fooTup = foo,
fooList = [foo]

print("test1:")
def rules(foofoo: str=None):
    if foofoo is None:
        print(foo + " rules! from a global scope.")
    else:
        print(foofoo + " rules! as an argument.")

rules()
rules(foo)
print("foo globally is still: {}".format(foo))


print("\ntest2:")
def print_global_foo():
    print("foo = " + foo)
    # foo = foo + rules   # UnboundLocalError!!!

print_global_foo()
print("foo globally is still: {}".format(foo))


print("\ntest3:")
def try_to_change_foo():
    try:
        foo = foo + " rules!"   # new local variable before assignment!
        print("foo = " + foo)
    except UnboundLocalError:
        print("threw UnboundLocalError: foo did not reference global foo")
    else:
        print("happy path does not happen")

try_to_change_foo()
print("foo globally is still: {}".format(foo))


print("\ntest4:")
def punt_on_changing_foo(foofoo: str):
    try:
        foo = foofoo + " rules!"   # new local variable
        print("foo = " + foo)
    except UnboundedLocalError:
        print("unhappy path does not happen")
        print("threw UnboundLocalError: but foo is just local?")
    else:
        print(foo + " but only locally")

punt_on_changing_foo(foo)
print("foo globally is still: {}".format(foo))


print("\ntest5:")
def get_foo_from_tup_globally():
    try:
        foo = fooTup[0] + " rules!"   # new local variable before assignment!
        print("foo = " + foo)
    except UnboundLocalError:
        print("threw UnboundLocalError: this does not happen")
    else:
        print("{} but indirectly and unchangeable".format(foo))

get_foo_from_tup_globally()
print("foo globally is still: {}".format(foo))


print("\ntest6:")
def get_foo_from_list_globally():
    try:
        fooList[0] = fooList[0] + " rules!"   # new local variable before assignment!
        print(fooList[0], end=" ")
    except UnboundLocalError:
        print("threw UnboundLocalError: this does not happen since fooList always global")
    finally:
        print("globally but discretely")

get_foo_from_list_globally()
print("foo globally is still: {}".format(foo))
print("but fooList globally is now: {}".format(fooList))


