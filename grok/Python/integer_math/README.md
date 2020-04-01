## Geoffrey's Boring Math Library
* Simple examples of Python libraries
* Example executables to exercise libraries.

### Source Code:

* [integer\_math.py](lib/integer_math.py): Integer mathematics library
* [func\_tools.py](lib/func_tools.py): Functional programming library
* [pythag3.py](bin/pythag3.py): Computes Pythagorean triples
* [ackermann.py](bin/ackermann.py): Computes Ackermann function

### Notes:

When developing this code, I usually use the root interger\_math
directory as my working directory.  I set
```
   $ export PYTHONPATH=../lib
   $ export PATH=$PATH:bin
```
so `python` and `pylint` can find these libraries and my shell can
find the executables.  I also have a [.pylintrc](.pylintrc) file
here so that variable names like `m`,`n`,`a`,`b` don't make `pylint`
complain too much.  Normally these would be horrible names, but
in the context of mathy integer functions and functional programming,
they do make sense.
