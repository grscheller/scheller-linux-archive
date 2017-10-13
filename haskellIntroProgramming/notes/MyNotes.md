# My Haskell notes:
Goal is to keep these notes sufficiently short to be able to wrap my head
around them, even at the price of them not being properly logically
grounded, perhaps even circular.  Notes taken from the course, the
Haskell 2010 refernce manual, or my general observations.

## Haskell Type System
* The type system partitions the universe of values and expressions into
  equivalence classes limiting how elements of these equivalence classes
  can be combined.
* "Variables" are bounded with the symbol `=` to immutable values, denoted
  by expressions.
* `=` is not an operator, it is part of the language, `(=) foo 5` does not work!

### Algebraic Data Types (ADT)
* All nullary constructors are "grounded."
* If `n` is grounded, then so is S n.
* Non-grounded expreessions exist,
  ```
     data NaturalNumber = Zero | S NaturalNumber
     infinity = S infinity
  ```

## Scoping

### Progam Structure
1. Top most level a Haskell program is a set of modules.
   * Provide way to control namespaces.
   * Allows re-use of software in large projects.
2. Top level of a module consists of a collection of declarations.
   * Ordinary values
   * datatypes
   * type classes
   * fixity information
3. Next lower level are expressions.
   * An expression denotes a value.
   * Expressions have static types.
   * Heart of Haskell programming "in the small."
4. Haskell has no statements.
   * Program execution involve evaluating expressions.
   * Not executing statements which change data states.
   * Expression evaluation involves λ-substitution and pattern matching.
   * Like mathematical substitution; equational reasoning.
   * Though they can model it, programs don't change state.
5. Bottom level is Haskell's lexical structure.
   * Captures concrete lexical structure of Haskell programs in text files.

### let vs. where
* Used to bind names to local expressions.
* Difference is more subtle than just whether definitions come first or last.
* Deep within the Haskell kernel, both implemented as λ-expressions.
1. **`let` construct**
   * Part of Haskell's expression syntax.
   * Comes before the expression.
   * Does not extend past guards.
2. **`where` construct**
   * Part of Haskell's definition syntax.
   * Comes after the definition.
   * Scope extends over all guards in the expressions making up the definition.
