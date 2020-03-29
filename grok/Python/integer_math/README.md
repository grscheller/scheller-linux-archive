## A Python Integer Math Library
* A simple example of a Python library
* with example executables to go with it.

### Source Code:

* [integer\_math.py](lib/pure_math.py): Library for integer mathematics
* [pythag3.py](bin/pythag3.py): Computes Pythagorean triples
* [ackermann.py](bin/ackermann.py): Computes Ackermann function

### Notes:

For executables to find the pure\_math library, when testing from
within the bin directory,
```
   $ export PYTHONPATH=../lib
```

The file [.pylintrc](.pylintrc)
is to stop pylint from complain about variable
names like `m`,`n`,`a`,`b`.  Normally these would be horrible names,
but in the context of mathy integer functions, they do make sense. 
You need to edit files with vim (assuming you are using with the
Syntastic plug-in which uses pylint) from the `integer_math` root
directory.
