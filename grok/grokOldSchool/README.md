# Capture various "traditional" programming paradigms:

Create examples of various pre-1990 imperitive programming techniques.
Not necessarily resticting myself to pre-1990 languages or tools.  Each
subtopic tries to either answer simples questions or provide simple examples.
Also to provide simple templates as starting poins for more involved projects.
It is always easier to edit than create from scratch.

## 1. [Hello World](HelloWorld/):
Tradition demands we start with the canonical Hello World program.

Goal is to write short, but not an irrationally short, programs that
when run prints the greeting "Hello, world!"

## 2. [Over optimization](OverOptimization/):
C compilers can overly optimize code.

Example of the compiler doing the calculation and the program merely
printing a constant, and how to stop this.  Also, shows a legitamate
use of the precompiler in C code and Bourn shell scripting techniques
within a makefile.

Build 4 binaries from one source file, time them, and clean up:
```
   $ make
   $ make timeit
   $ make clean
```
