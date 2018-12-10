# Old School Programming
* Capture various "traditional" programming paradigms
* Create examples of various pre-1990 imperitive programming techniques
* Not necessarily resticted to pre-1990 languages or tools

## 1. [C Systems Programming](SystemsProgramming/):
I have always admired W. Richard Steven's books on Unix System
programming.  Back in the early 1990's, I thought someday I would
work my way through them.  Well, I have finally gotten around to
it, in 2018.  I want to investigate the use of bloody edge C++
as a general purpose language, but before I do so, I thought I'd
better learn what it is I am trying to replace.  Thanks to POSIX
standards, Steven's books are as relevant today as they were back
in the 1990's.

The books I am using are
"Advanced Programming in the UNIX Environment"
and
"UNIX Network Programming"
volumes 1 & 2.

Using `gcc -std=c90` to match the standard used by these books.
* __myErrorHandlers.c__: Common error handling routines.
* __simple_ls.c__: Application to display file names in a directory.
* __myUnbufferedCat.c__: Lower level IO defined in unistd.h
* __myBufferedCat.c__: Buffered IO defined in stdio.h
* __pidInfo.c__: Process ID information

## 2. [C++ to Infinity and Beyond](C++InfinityAndBeyond1/):
ANSI-C needs to be replaced.  Unfortunately, it is good enough
never to be replaced.  I feel C++, properly done, could be a
safer and socially palitable replacement.  This project was
inspired by Bjarne Stroustrup's "A Tour of C++" Second Edition.

Building on the bloody edge with `gcc -std=c++2a`.
* __hw.cpp__: Buzz Lightyear's Hello World C++ program.

## 3. [XWindows](XWindows):
Examples of low level XWindows coding.

## 4. [C Language Exempars](CExemplars/):
Short examples of best practices for ANSI-C coding.  A lot of factoids
in the comments.
* __hw.c__: Traditional Kerrigan & Richie Hello World C program.
* __removeSpaces.c__: O(n) string manipulation
* __sumit.c__: Compiler over-optimization

The last one is an example of the compiler doing the
calculation and the program merely printing a constant.
and how to stop this from happening by using of C language
volitile` keyword.  Also, an example of Bourn shell
scripting techniques within a makefile.

Build 4 binaries from one source file, time them, and clean up:
```
   $ make
   $ make timeit
   $ make clean

### Example run:
```$ make timeit
   gcc sumit.c -Wall -std=c99 -o sumIt
   gcc sumit.c -Wall -std=c99 -O2 -o sumItO2
   gcc sumit.c -Wall -std=c99 -DVOLATILE -o sumItV
   gcc sumit.c -Wall -std=c99 -DVOLATILE -O2 -o sumItVO2
   for bb in sumIt sumItO2 sumItV sumItVO2;\
   do\
       echo -ne "\n$bb: ";\
       time ./$bb;\
   done

   sumIt: VOLATILE not set: 5000000050000000

   real	0m0.266s
   user	0m0.266s
   sys	0m0.000s

   sumItO2: VOLATILE not set: 5000000050000000

   real	0m0.001s
   user	0m0.001s
   sys	0m0.000s

   sumItV: VOLATILE  is set: 5000000050000000

   real	0m0.174s
   user	0m0.173s
   sys	0m0.000s

   sumItVO2: VOLATILE  is set: 5000000050000000

   real	0m0.160s
   user	0m0.160s
   sys	0m0.000s
```
```
Note how "fast" the second run is.  All that is happenng
is the binary is just spitting a constant out.  Currious
that just making a variable volatile speeds up the code.
