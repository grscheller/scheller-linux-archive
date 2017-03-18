# pureMath.py
A simple example of a Python library and executable.

## Source Code:

### lib/pureMath.py
   * Library for integer mathematics.

### bin/pythag3.py
   * Program to compute Pythagorean triples

## Notes:
Known to work with both Python 2.7 and Python 3.2+ 

I wrote these for Python 3, but tried to keep 
Python 2.7 backward compatibility.

I use the PYTHONPATH environment variable so 
that the executable can find its library.  Most 
Phython Scientific Stacks, like Enthought and 
Anaconda, frown on using this environment variable. 
Its use here is harmless since the code is 
both 2.7/3 compatible.

The .gitignore file is set up to ignore compiled 
Python byte code:
```
    __pycache__        for Python 3
    *.pyc              for Python 2.7
```
