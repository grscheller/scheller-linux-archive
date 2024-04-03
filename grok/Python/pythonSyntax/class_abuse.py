#!/usr/bin/env python

"""Test what can be done with Python classes

I am exploring just the base language, not extra-typing information provided
by the typing module allowing LSP enabled tooling to enforce certain
relationships.

Python is a pure OOP language in the traditional (Smalltalk) sense. Everything
is an objects:

  - built in and user defined values: 42, 3.14159, 
  - builtin and user defined types are values: int, double, list, tuple,  str
  - functions are first class values and are object!
  - a class is an objects too, its type (class) is called its "metaclasses"
  - modules are objects too

What in Python is NOT an object? References, sometimes confusingly called
"variables." References are not areas in memory to "store" values, they are
labels that we can swap onto values (objects).

The following is my attempt to "grok" classes, it is not intended to be any sort
of "best practices."

A CDL trainer once gave me the following advice, "Just because you are capable
of steering a bus using just your feet does not mean that it is a good idea to
do so."
"""
## Explore a minimal class higher hierarchy, Notice how "function syntax" is
#  overloaded to denote inheritance,

# A minimal class
class Car():
    pass

# A minimal derived class
class CarBrand(Car):
    pass

# Another minimal derived class
class CarModel(CarBrand):
    pass

# Instantiate class via "calling" class as a function
# - actually we are calling a "default" constructor
rental = Car()
mitsubishi = CarBrand()
outlander = CarModel()

# The type builtin function returns the actual type of the object, the runtime
# behavior of any actual class hierarchy (contravariant, covariant, or
# invariant) depends on how the "properties" and "methods" are structured.

assert type(rental) is Car
assert type(rental) is not CarBrand
assert type(rental) is not CarModel

assert type(mitsubishi) is not Car
assert type(mitsubishi) is CarBrand
assert type(mitsubishi) is not CarModel

assert type(outlander) is not Car
assert type(outlander) is not CarBrand
assert type(outlander) is CarModel
