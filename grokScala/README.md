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
make sure you launch sbt from the root grokScala directory.

## 2. Multiple packages in single build: [grok](grok/)
The above multiple projects (my term) in the hierarchical build compile<br>
scala packages which are independent of each other in the sense that<br>
dependencies between them must be configured like other local external<br>
packages.  The grok project shows how to compile multiple packages in a way<br>
that allows SBT to "out-of-the-box" handle package dependencies among them.

## 3. Building scala code to run as a "Java App": [onejar](onejar/)
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

## 4. Scala's "splat" equivalents: [splat.scala](splat/splat.scala)
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
To run outside sbt build,,
```
   $ scala Splat.splat  
```

## 5. Scala code blocks: [codeblocks](codeblocks/)
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
was first run, but statements after the lambda get executed each time `dog`<br>
was executed.  Though the  return value of `dog` is indead `()`.  Actually,<br>
the lambda extends to the end of the code block.

This means that the use of `self` as a synonym for `this` in class and trait<br>
definitions may not just be "Syntactic Sugar" but have deeper semantic meaning.
```
   trait Foo[+A] { self =>
      ...
   }
```

## 6. Scala oop: [oop](oop/)
Exploring Scala OOP features.  When an OOP model fits the problem, it is not<br>
bad.  I just don't like it when it handcuffs me.

## 7. Parallelism with Scala: [parallelism](parallelism/)
Exploring multithreading/concurrent constructs in Scala.

It seems that Actors from the Scala standard library have been dropped in<br>
favor of akka Actors.  Also, the standard library now has a Promises<br>
implementation, which is very different that the one removed from Scalaz.

## 8. State Monad: [grokScala.grok.state](grok/src/main/scala/grok/state/State.scala)
An implementation of the State Monad I took from my version of
the [fpinscala package](../fpinscala).

## 9. Package objects: [grokScala.grok.rand](grok/src/main/scala/grok/packageWide/)
In scala, types cannot be defined outside of classes/objects.  They are<br>
features that is part of Scala's OO system - see page 457 of Oderski's<br>
Programming in Scala, 3rd edition, on path dependent types.  Types are<br>
members just like defs, vals, and vars.  Functional programmers use this<br>
feature to implement type aliases.

Scala provides a mechanism to define a type alias at the package level.<br>
They are defined in a package object.  There can only be one package object<br>
per package.  By convention, the package object is put in a file named<br>
package.scala located in the root directory of the package.  The package<br>
object itself is part of the package's parent package.

In the `grokScala.grok` package, `Rand[A]`, is a type alias for
`state.State[RNG, A]`<br>
It is defined package-wide in the file
[package.scala](grok/src/main/scala/grok/rand/package.scala) located in the
`rand`<br>
package root directory, grok/src/main/scala/grok/rand/. 
* Used to define the Rand type alias at the rand package level.
* Using `val Rand = grokScala.grok.state.State` so the compiler can find the companion object.
* User code needs to use `new` key word to distinguish constructor from the `State.apply` method.

This is my original implimentation of `fpinscala.state.rand.Rand`.  I plan<br>
to reimplemented it there as a case class containing a `State(RNG,A)`.
