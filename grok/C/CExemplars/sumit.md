# C Compiler Over-Optimization

* Depending how you compile it, [sumit.c](sumit.c) is an example of the compiler
  doing the calculation and the program merely printing a constant
* How to stop this from happening by using of C language `volitile` keyword
* Also, illustrates POSIX shell scripting techniques within a makefile

Build 4 binaries from one source file, time them, and clean up:

```bash
   make sumIt
   make timeit
   make clean
```

Example run:

```bash
   $ make timeit
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

   real 0m0.266s
   user 0m0.266s
   sys  0m0.000s

   sumItO2: VOLATILE not set: 5000000050000000

   real 0m0.001s
   user 0m0.001s
   sys  0m0.000s

   sumItV: VOLATILE  is set: 5000000050000000

   real 0m0.174s
   user 0m0.173s
   sys  0m0.000s

   sumItVO2: VOLATILE  is set: 5000000050000000

   real.0m0.160s
   user.0m0.160s
   sys  0m0.000s
```

Note how "fast" the second run is.  All that is happenng
is the binary is just spitting a constant out.  Currious
that just making a variable volatile speeds up the code.
