## A Python Pure Math Library
* A simple example of a Python library
* Example executables to go with it.

### Source Code:

* [pure\_math.py](lib/pure_math.py): Library for integer mathematics
* [pythag3.py](bin/pythag3.py): Program to compute Pythagorean triples

### Notes:

So that bin Python executables find the pure\_math library, when testing from
within the bin directory,

  export PYTHONPATH=../lib

The .gitignore file is set up to ignore compiled Python byte code:
```
    __pycache__        for Python 3
```

The lib/.pylintrc file is to stop pylint from complain about variable
names like `m`,`n`,`a`,`b`.  Normally these would horrible names, but
in the contect of mathy integer functions and pythagorean triples,
they do make sense. 
