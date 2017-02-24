# Basic Scala Language Constructs:

1. ## Scala's "splat" equivalents: splat.scala
     Languages like Python and Ruby have a syntactic
     sugar to pass an array or tuple into a fixed or
     variable-arity function.

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

     It makes sense to "splat" variable length datastructures into
     variadic functions.  Also to "splat" tuples into fixed-arity
     functions.  Other combinations seem to me more naturally
     handled by unpacking and repacking.

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

     To compile the code, 
     ```
        $ scalac splat.scala
     ```
     To run,
     ```
        $ scala grockScala.splat  
     ```
