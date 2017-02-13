# Basic Scala Language Constructs:

## Scala's "splat" equivalents: splat.scala
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

  To compile the code, 
  ```
     $ scalac splat.scala
  ```
  To run,
  ```
     $ scala scalaSyntax.splat  
  ```
  It makes sense to "splat" variable length datastructures into
  variadic functions.  Also to "splat" tuples into fixed-arity
  functions.  
