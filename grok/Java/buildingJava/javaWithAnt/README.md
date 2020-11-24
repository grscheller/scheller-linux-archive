# Java with Ant

Ant is one of the older build tools for Java.  Make had the
problem that different makefile rules were needed for cmd.com
on Windows and /bin/sh on Unix.  Think of Ant as a platform
independent version of make, without pattern matching, which
uses XML configuration files.

## Targets

```
   $ ant clean
   $ ant clobber
   $ ant compile
   $ ant jar
```

## To run

Change directory to the base directory, the one with the build.xml
file.

From class files:

```
   $ export CLASSPATH=classes
   $ java IsPrime 37 111
   37 is prime.
   111 is not prime.
```

From jar file:

```
   $ java -cp javaWithAnt.jar IsPrime 100 191
   100 is not prime
   191 is prime

   $ java -cp javaWithAnt.jar Carl 100
   Sum of 1 to 100 is 5050

   $ java -jar Carl.jar 34
   Sum of 1 to 34 is 595
```
