# Basic Scala Language Constructs:

Explore Scala language and build constructions.<br>

## 1. Hierarchical SBT build.
To compile all projects,
```
   $ sbt compile
```
compile just one project,
```
   $ sbt splat/compile
```
run one project,
```
   $ sbt splat/run
```

## 2. Building scala code to run as a "Java App":
Builds scala code into a "fat" jar file so that all an end user needs is<br>
a functioning java runtime environment.  This will includes everything in<br>
the jar file, including the Scala runtime libraries.<br>

This uses [stb-assembly](https://github.com/sbt/sbt-assembly)<br>

To build the fat jar file, 
```
   $ sbt onejar/compile
   $ sbt onejar/assembly
```
To run,
```
   $ java -jar onejar/target/scala-2.12/onejar-assembly-0.1-SNAPSHOT.jar 
   Hello World!
```
All it is is a stupid hello world program, with added complexity to<br>
illustrate how to indicate the default main class.<br>

To get this to work, I had to name the default main class Main.  Also, I<br>
noticed that I had to manually trigger off a compile for assembly to pick<br>
the latest source code changes.

## 3. Scala's "splat" equivalents: [splat.scala](splat/splat.scala)
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

## 4. Scala code blocks: [codeblocks.scala](codeblocks/codeblocks.scala)
Scala code blocks are interesting closures.  This project is to explore<br>
code blocks as functions.

I found something I thought peculiar while in the scala REPL:<br>
```
    scala> val dog = {
         |   println("This is run just once.")
         |   var fido = 5
         |   (a: Int) => fido = fido + a
         |   println("fido is " + fido)
         | }
    This is run just once.
    dog: Int => Unit = <function1>
    
    scala> dog(2)
    fido is 7

    scala> dog(5)
    fido is 12

    scala> val hold = dog(10)
    fido is 22
    hold: Unit = ()
```
If you put a single lambda in the middle of a code block, my expectation<br>
was that the lambda would be ignored and I'd get back just `()` as the<br>
the value of `dog`, which comes from the last println function call in<br>
the code block.  Instead, its return value is a `function1` where the<br>
statements after the lambda become part of the `function1`.  Statements<br>
before the lambda were executed only once as expected when the code block<br>
was first run, but statements after the lambda get executed each time `dog`
was executed.  Though the  return value of `dog` is indead `()`.


