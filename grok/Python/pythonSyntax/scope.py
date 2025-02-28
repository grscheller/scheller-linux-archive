#!/usr/bin/env python

"""Test scoping rules to see how an inner scope can affect an outer scope.

Python will try and stop you from doing this stupid thing, but you can still
do it you use a mutable container for the value you want to change. 
"""

foo = "Foofoo"
fooTup = foo,
fooList = [foo]

print("test1:")
def rules(foofoo: str | None=None):
    if foofoo is None:
        print(foo + " rules! with a global reference.")
    else:
        print(foofoo + " rules! as an argument.")

rules()
rules(foo)
print("from global scope: foo = {}".format(foo))


print("\ntest2:")
def print_global_foo_doit(doit: bool):
    try:
        print("foo = " + foo)
        if doit:
            foo = foo + rules   # UnboundLocalError!!!
            print("locally foo = {}".format(foo))
        else:
            pass
    except UnboundLocalError:
        print("threw UnboundLocalError: doit = {}".format(str(doit)))
    else:
        print("happy path does not happen")

print_global_foo_doit(False)
print_global_foo_doit(True)
print("from global scope: foo = {}".format(str(foo)))


print("\ntest3:")
def print_global_foo_doit2(doit: bool):
    try:
        print("foo = " + foo)
        if doit:
            print("locally foo = {}".format(foo))
        else:
            pass
    except UnboundLocalError:
        print("threw UnboundLocalError: doit = {}".format(str(doit)))
    else:
        print("happy path now happen")

print_global_foo_doit2(False)
print_global_foo_doit2(True)
print("from global scope: foo = {}".format(str(foo)))


print("\ntest4:")
def punt_on_changing_foo(foofoo: str):
    try:
        foo = foofoo + " rules!"   # new local variable
        print("locally: foo = " + foo)
    except UnboundLocalError:
        print("unhappy path does not happen")
        print("threw UnboundLocalError: but foo is just local?")
    else:
        print(foo + " but only locally")

punt_on_changing_foo(foo)
print("from global scope: foo = {}".format(str(foo)))


print("\ntest5:")
def get_foo_from_tup_globally():
    try:
        foo = fooTup[0] + " rules!"
        print("foo = " + foo)
    except UnboundLocalError:
        print("threw UnboundLocalError: this does not happen")
    else:
        print("{} but indirectly and unchangeable".format(foo))

get_foo_from_tup_globally()
print("foo globally is still: {}".format(foo))
print("fooTup globally is still: {}".format(str(fooTup)))


print("\ntest6:")
def get_foo_from_list_globally():
    try:
        fooList[0] = fooList[0] + " rules"
        print("locally, fooList is {}".format(str(fooList)))
    except UnboundLocalError:
        print("threw UnboundLocalError: this does not happen since fooList always global")
    else:
        print("global fooList was manipulated in local context")

get_foo_from_list_globally()
print("globally: fooList = {}".format(fooList))


print("\ntest7:")
def get_foo_from_list_globally_into_local_copy_wrong():
    try:
        fooList = list(fooList)   # new local variable before assignment!
        fooList[0] = fooList[0] + " absolutely"
        print("local fooList is {}".format(str(fooList)))
    except UnboundLocalError:
        print("threw UnboundLocalError: ooops!!!")
    else:
        print("global fooList was referenced, strictness of list?")
        print("local ")

get_foo_from_list_globally_into_local_copy_wrong()
print("globally: fooList = {}".format(fooList))


print("\ntest8:")
def get_foo_from_list_globally_into_local_copy_right():
    try:
        fooList1 = list(fooList)   # new local variable before assignment!
        fooList1[0] = fooList[0] + "!!!"
        print(fooList1[0], end=" ")
    except UnboundLocalError:
        print("threw UnboundLocalError: doesn't happen since fooList1 new local")
    finally:
        print("local fooList was manipulated in local context")

get_foo_from_list_globally_into_local_copy_right()
print("globally: fooList = {}".format(fooList))
