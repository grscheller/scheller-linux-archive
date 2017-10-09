# My Haskell notes:
Goal is to keep these sufficiently short to be able to wrap my head
around them, even at the price of them not being properly logically
grounded.

## Haskell Type System
* The type system partitions the universe of values and expressions into
  equivalence classes limiting how elements of these equivalence classes
  can be combined.
* "variables" (names - denoted by initial lower case) are bounded to
  values, denoted by expressions, by "binary expression" `=`.
* `=` is not an operator, it is part of the language, `(=) foo 5` does not work!

### Algebraic Data Types (ADT)
* All nullary constructors are "grounded."
* If `n` is grounded, then so is S n.
* Non-bounded expreessions exist,
  ```
     data NaturalNumber = Zero | S NaturalNumber
     infinity = S infinity
  ```
