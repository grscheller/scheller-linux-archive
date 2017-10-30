# Basic Scala Language Constructs:

Explore Scala language and build constructions.

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
The above multiple projects (my term) in the hierarchical build compile
scala packages which are independent of each other in the sense that
dependencies between them must be configured like other local external
packages.  The grok project shows how to compile multiple packages in a way
that allows SBT to "out-of-the-box" handle package dependencies among them.

## 3. Scala's "splat" equivalents: [Splat.scala](splat/Splat.scala)
Languages like Python and Ruby have a syntactic sugar to pass an
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

It makes sense to "splat" variable length datastructures into variadic
functions.  Also to "splat" tuples into fixed-arity functions.  Other
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
   $ scalac Splat.scala
```
To run outside sbt build,,
```
   $ scala grokScala.splat.Splat  
```

## 4. Scala code blocks: [codeblocks](codeblocks/)
Scala code blocks are interesting closures.  They can contain state
and but are not first class objects.

This project is to explore to what extend code blocks behave like functions.

I found something I thought peculiar while in the scala REPL:
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
If you put a single lambda in the middle of a code block, my expectation
was that the lambda would be ignored and I'd get back just `()` as the
value of `dog`, which comes from the last println function call in
the code block.  Instead, it returned value a `function1` where the
statements after the lambda become part of the `function1`.  Statements
before the lambda were executed only once as expected when the code block
was first run, but statements after the lambda get executed each time `dog`
was executed.  I was totally confused.

The problem was that the lambda actually extends to the end of the code block.
λ-functions extend as far "to the right" as possible.  I took "right" too
literally.  Maybe we should say that λ-functions extend as far "syntactically"
as possible.

Code blocks are not "first class objects" in the functional programming
sense.  They are just  "thunks."  When you put the λ-function at the end,
you are returning a "first class object" from the thunk.  Thus, `dog` is
not the "codeblock" but just the returned lambda,

If the execution of the codeblock were expensive, a useful technique would be
to define it as a `lazy val'. 

Codeblocks, like any good closures, can contain state.

### Related topics:
* case blocks, without a match, actually define partial functions.  These
  can be extremely useful when used with collect methods.
* Sort of related, the use of `self` as a synonym for `this` in class and
  trait definitions.  These may be more than just "Syntactic Sugar" but
  have some deeper meaning.  The use of `self` returns/references the
  class/trait itself within the class/trait.  Since classes/traits do not
  use `=`, like a method can, their "return values" definately 
  not a λ-function, but instances of the class/trait.

```
   trait Foo[+A] { self =>
      ...
   }
```

## 5. Sort a polymorphic ordered list: [Sort.scala](sort/Sort.scala)
Compare this to the Haskell implementation in
[here](../grokHaskell/haskellIntroProgramming/examples/Utilities.hs).

## 6. Scala oop: [oop](oop/)
Exploring Scala OOP features.  When an OOP model fits the problem, it is not
bad.  I just don't like it when it handcuffs me.

## 7. Parallelism with Scala: [parallelism](parallelism/)
Exploring multithreading/concurrent constructs in Scala.

It seems that Actors from the Scala standard library have been dropped in
favor of akka Actors.  Also, the standard library now has a Promises
implementation, which is very different that the one removed from Scalaz.

## 8. State Monad: [grokScala.grok.state](grok/src/main/scala/grok/state/State.scala)
An implementation of the State Monad I took from my version of
the [fpinscala package](../fpinscala).

## 9. Package objects: [grokScala.grok.rand](grok/src/main/scala/grok/rand/)
In scala, types cannot be defined outside of classes/objects.  They are
features that are part of Scala's OO system - see page 457 of Oderski's
Programming in Scala, 3rd edition, on path dependent types.  Types are
members just like defs, vals, and vars.  Functional programmers use this
feature to implement type aliases.

Scala provides a mechanism to define a type alias at the package level.
They are defined in a package object.  There can only be one package object
per package.  By convention, the package object is put in a file named
package.scala located in the root directory of the package.  The package
object itself is part of the package's parent package.

In the `grokScala.grok` package, `Rand[A]`, is a type alias for
`state.State[RNG, A]`
It is defined package-wide in the file
[package.scala](grok/src/main/scala/grok/rand/package.scala) located in the
`rand`
package root directory, grok/src/main/scala/grok/rand/. 
* Used to define the Rand type alias at the rand package level.
* Using `val Rand = grokScala.grok.state.State` so the compiler can find the companion object.
* User code needs to use `new` key word to distinguish constructor from the `State.apply` method.

This is my original implimentation of `fpinscala.state.rand.Rand`.
I reimplemented it there as a case class containing a `State(RNG,A)`.
