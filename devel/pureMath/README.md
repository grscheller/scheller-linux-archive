A simple example of a Python library and executable.

Known to work with both Python 2.7 and Python 3.2+

I wrote these for Python 3, but tried to keep
Python 2.7 backward compatibility.

I use the PYTHONPATH environment variable so that the
executable can find its library.  Most Phython Scientific
Stacks, like Enthought and Anaconda, frown on using this
environment variable.  It use here is harmless since the
code is 2.7/3 compatible.

The .gitignore file is set up to ignore compiled Python
byte code:

  __pycache__/             for Python 3
  *.pyc and __pycache__/   for Python 2.7

