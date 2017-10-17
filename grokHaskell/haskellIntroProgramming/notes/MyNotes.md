# My Haskell notes:
Goal is to keep these notes sufficiently short to be able to wrap my head
around them, even at the price of them not being properly logically
grounded, perhaps even circular.  Notes taken from the course, the
Haskell 2010 reference manual, or my general observations.

## Haskell
* Haskell is a strongly-type, lazy, pure, functional language.
* Types are compile time entities.
  + Values need not have the run time overhead of encoding their types.
  + No run time dispatch overhead.
  + No run time type incompatibilities.
  + No run time "duck typing" crashes or unintended type conversions.
* Lazy in the sense that values are not computed unless needed/forced.
* Pure in the sense of:
  + Values are immutable.
  + No statements, program with referentially transperant expressions.
  + Functions have no side effects.
  + Functions are first class values.

## Haskell Type System
* Strong, flexible type system with powerful type inference algorithms.
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
* Used to bind names in local scopes.
* Difference is more subtle than just whether definitions come first or last.
* Deep within the Haskell kernel, both probably implemented as λ-expressions.
1. **The `let ... in ...` expression**
   * Part of Haskell's expression syntax.
   * The `let` "comes before the expression," means the `let` bindings only
     apply to the subexpression after the `in` clause.
   * Does not "extend past guards," i.e. applies to just the subexpression, 
     not to any enclosing definitions.
2. **The `where` clause of a definition**
   * Part of Haskell's definition syntax.
   * Comes after the definition.
   * Scope extends over all guards within the definition.
