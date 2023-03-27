# Java with Make

Using make as the build tool really helped me learn the
underlying java and jar commands used to build java
code.  Also a good opportunity to relearn make pattern
rules.

## Targets

```bash
   $ make
   javac -d classes Carl.java
   javac -d classes IsPrime.java

   $ make jar
   jar cfe Carl.jar Carl -C classes Carl.class
   jar cfe IsPrime.jar IsPrime -C classes IsPrime.class
   jar cf javaWithMake.jar -C classes

   $ make clobber
   rm -fr classes
   rm -f Carl.jar IsPrime.jar javaWithMake.jar

   $ make IsPrime.jar
   javac -d classes IsPrime.java
   jar cfe IsPrime.jar IsPrime -C classes IsPrime.class

   $ make jar
   javac -d classes Carl.java
   jar cfe Carl.jar Carl -C classes Carl.class
   jar cf javaWithMake.jar -C classes .
```

## To run

Change directory to the base directory, the one with the makefile.

From class files:

```bash
   $ export CLASSPATH=classes
   $ java IsPrime 2 721692259 721692261
   2 is prime.
   721692259 is prime.
   721692261 is not prime.
```

From jar files:

```bash
   $ java -cp javaWithMake.jar Carl 10
   Sum of 1 to 10 is 55

   $ java -jar Carl.jar 100
   Sum of 1 to 100 is 5050
```
