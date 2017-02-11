# Basic Scala Language Constructs:

## Scala's "splat" equivalents: splat.scala
  Languages like Python have a syntactic sugar to
  pass an array or tuple into a fixed or variable
  arity function.
  ```
     >>> def f(m,n,p):
     ...   return m*n + p
     ...
     >>> f(2,3,4)
     10
     >>> x = (2,3,4)
     >>> f(*y)
     10
  ```
  The code in splat object in splat.scala explores
  similar constructs in scala.

  To compile the code, 
  ```
     $ scalac splat.scala
  ```
  To run,
  ```
     $ scala scalaExamples.splat  
  ```
  Currently, code is just a start.
