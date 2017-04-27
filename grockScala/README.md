# Basic Scala Language Constructs:

Explore Scala language and build constructions.<br><br>
For now separate SBT builds, heading toward a hierarchical SBT build.

## 1. Scala's "splat" equivalents: [splat.scala](splat/splat.scala)
Languages like Python and Ruby have a syntactic sugar to pass an<br>
array or tuple into a fixed or variable-arity function.

In Python:
```
   >>> def f(m,n,p):
   ...   return m*n + p
   ...
   >>> f(2,3,4)
   10
   >>> x = (2,3,4)
   >>> f(*x)
   10
```
The code in the splat object in splat.scala explores
similar constructs in scala.

It makes sense to "splat" variable length datastructures into variadic<br>
functions.  Also to "splat" tuples into fixed-arity functions.  Other<br>
combinations seem to me more naturally handled by unpacking and repacking.

To "splat" a variable length datastructure into a variadic function:
```
   val a1 = bar(xs: _*)
```
To "splat" a tuple into a fixed-arity function:
```
   val a2 = foo _ tupled (42, 3.0, "Fred")
```
To "splat" a tuple into a curried function:
```
   val a3 = Function.uncurried(fooC _) tupled (42, 3.0, "fred")
```

To compile outside the sbt build, 
```
   $ cd splat
   $ scala grockScala.Splat.splat
```
To run,
```
   $ scala Splat.splat  
```

## 2. Building scala code to run as a "Java App":
Builds scala code into a "fat" jar file so that all an end user needs is<br>
a functioning java runtime environment.  This will includes everything in<br>
the jar file, including the Scala runtime libraries.<br>

This uses sbt-assembly.<br>

To build the fat jar file, 
```
   $ cd onejar
   $ sbt assembly
```
To run,
```
   $ java -jar target/scala-2.12/onejar-assembly-0.1-SNAPSHOT.jar
```
All it is is a stupid hello world program.
