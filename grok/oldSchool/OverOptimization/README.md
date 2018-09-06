# Over optimization.
C compilers can overly optimize code.

* Example of compiler doing the calculation and the program merely
  printing a constant.
* How to stop this from happening.
* Use of C language volitile keyword.
* Example of a legitamate use for the C precompiler.
* Example of Bourn shell scripting techniques within a makefile.

Build 4 binaries from one source file, time them, and clean up:
```
   $ make
   $ make timeit
   $ make clean
```

## Example run:
```
   $ make timeit
   gcc main.c -Wall -o sumIt
   gcc main.c -Wall -O2 -o sumItO2
   gcc main.c -Wall -DVOLATILE -o sumItV
   gcc main.c -Wall -DVOLATILE -O2 -o sumItVO2
   for bb in sumIt sumItO2 sumItV sumItVO2;\
   do\
       echo -ne "\n$bb: ";\
       time $bb;\
   done

   sumIt: VOLATILE not set: 5000000050000000

   real	0m0.268s
   user	0m0.267s
   sys	0m0.000s

   sumItO2: VOLATILE not set: 5000000050000000

   real	0m0.001s
   user	0m0.000s
   sys	0m0.000s

   sumItV: VOLATILE  is set: 5000000050000000

   real	0m0.172s
   user	0m0.171s
   sys	0m0.000s

   sumItVO2: VOLATILE  is set: 5000000050000000

   real	0m0.158s
   user	0m0.156s
   sys	0m0.001s
```
* Note how "fast" the second run is.
  All that is happenng is the binary is just spitting a constant out.
* Currious that just making a variable volatile speeds up the code.
