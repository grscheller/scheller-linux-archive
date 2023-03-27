# Daddy's Boring Math Library

* Simple examples of Python modules
* Example executables to exercise modules
* Name for this project was suggested by my 13 year old daughter Mary

## Source Code

* [integer\_math.py](lib/integer_math.py): Integer Mathematics Module
* [func\_tools.py](lib/func_tools.py): Functional Programming Module
* [pythag3.py](bin/pythag3.py): Computes Pythagorean Triples
* [ackermann.py](bin/ackermann.py): Computes Ackermann Function

## Notes

When developing code like this form the command line, I usually use
the root `boring_math` directory as my working directory.  I set as
relative paths

```bash
   $ export PYTHONPATH=lib:../lib
   $
   $ export PATH=$PATH:bin
   $
```

so `python` and `pylint` can find these libraries and my shell can
find the executables.  I also have a [.pylintrc](.pylintrc) file
here so that variable names like `m`, `n`, `a`, `b` don't make `pylint`
complain too much.  Normally these would be horrible names but,
in the context of mathy integer functions and functional programming,
they do make sense.

Well, Neovim using Pyright as the LSP server does not follow
relative paths.  To get around this, I created a
[pyrightconfig.json](pyrightconfig.json) in the root boring_math
directory.

The next step is to figure out how to both install the library locally
in a development virtual environment, and globally as a tool to
explore mathy and scientific things.  I plan to use Pipenv for this,
at least for the first step.
