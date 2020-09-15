## Java with Ant

### Targets
```
   $ ant clean
   $ ant clobber
   $ ant compile
   $ ant jar
```
### To run
```
   $ cd .../scheller-linux-archive/grok/Java/javaWithAnt
```
From class files:
```
   $ export CLASSPATH=./classes
   $ java IsPrime 37 111  
   37 is prime.
   111 is not prime.
```
From jar file:
```
   $ export CLASSPATH=./IsPrime.jar
   $ java IsPrime 1111111111111111111 703
   1111111111111111111 is prime.
   703 is not prime.
```
