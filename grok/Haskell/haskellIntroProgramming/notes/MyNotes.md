# My Haskell Notes

Goal is to keep these notes sufficiently short to be able to wrap my head
around them, even at the price of them not being properly logically
grounded, perhaps even circular.  Notes taken from the course, the
Haskell reference manuals, or my general observations.

## Haskell

1. Haskell is a strongly-type, lazy, pure, functional language.
    * Types are compile time entities.
    * Values need not have the run time overhead of encoding their types.
    * No run time dispatch overhead.
    * No run time type incompatibilities.
    * No run time "duck typing" crashes or unintended type conversions.

2. Lazy in the sense that values are not computed unless needed/forced.

3. Pure in the sense of:
    * Values are immutable.
    * No statements, program with referentially transperant expressions.
    * Functions have no side effects.
    * Functions are first class values.

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
    * Comes after the definition, means applies to the entire definition.
    * Scope extends over all guards within the definition.

## Haskell Type System

* Strong, flexible type system with powerful type inference algorithms.
* The type system partitions the universe of values and expressions into
  equivalence classes limiting how elements of these equivalence classes
  can be combined.
* "Variables" are bounded with the symbol `=` to immutable values, denoted
  by expressions.
* Once assigned, a variable is immutable.  Can have different values in
  different instantiations of an expressions.
* `=` is not an operator, it is part of the language, `(=) foo 5` does not work!

### Algebraic Data Types (ADT)

* All nullary constructors are "grounded."
* If `n` is grounded, then so is `S n`.
* Non-grounded expreessions exist,

   ```
      data NaturalNumber = Zero | S NaturalNumber
      infinity = S infinity
   ```

#### Simplified versions of predefined ADTs

These types are either predefined into the Prelude, or they are "cooked"
into the runtime implementation.

1. Unit type:
    * The Unit type and Unit type constuctor.

      ```
         data () = () deriving (Eq, Ord, Show)
      ```

    * Can be thought as a 0-tuple, but actually an ADT.

2. Bool type:

    * Represents true and false boolean values.

      ```
         Bool type:
         data Bool
             = False
             | True
             deriving (Eq, Ord, Show)
      ```

3. Tuple type:
    * Two-tuple type class.

      ```
         data (,) a b = (,) a b
      ```

    * Slightly syntatic sugared, otherwise parser would hijack `(` and `)`.
    * Without the syntatic sugar, we would have to define it as

      ```
         data Pair a b = Pair a b deriving (Eq, Ord, Show)
      ```

    * Sugar to use and display like as done in mathematics,

      ```
         Prelude> (,) 2 3 == (2,3)
         True
         Prelude> show $ (,) 3 2
         "(3,2)"
      ```

    * Unlike Python, `,` is not an operator on its own.
    * The abstraction is that of a key-value pair.

      ```
         Prelude> fmap (\x -> x + 40) (1,2)
         (1,42)
      ```

4. List type:
    * An example of a recursive type.

      ```
         infixr 5 :
         data [] a
             = []
             | a : [] a
      ```

    * Like `(,)` the list type is a member of `Eq`, `Ord`, and `Show`.
    * The ordering is lexical, assuming that the underlying type a is ordered.
    * Syntaxic sugar to allow

      ```
         Prelude> foo a b = [a, b, a + b]
         Prelude> foo 1 2
         [1,2,3]
      ```

#### Deconstructiong types

With pattern matching,

```
   addMaybes (Just x) (Just y) = Just (x + y)
   addMaybes _ _               = Nothing
```

with guards,

```
   addMaybes mx my | Just x <- mx, Just y <- my = Just (x + y)
   addMaybes _  _                               = Nothing
```

and λ-abstractions can also deconstruct patterns

```
   (/(Right x) -> x)  -- run time error if pattern has wrong constructor
                      -- guards & multiple bindings not allowed.
```

## Haskell Syntax

Function application is most fundamental concept, so function
application is just done by justiposition.

```
   fun arg1 arg2 arg3
```

Some say arguments are "separated by spaces."  But

```
   fun(arg1)(arg2)(arg3)
```

would be parsed as justiposition too, but considered "bad style."

### Operators

#### Very few operators reserved by language syntax

In Haskell, most predefined operators are either just library functions
or are cooked into the parser, examples of these are:

```
   .., :, ::, =, \, |, <-, ->, @, ~, =>, --
```

You can go crazy and define your own operators, or even use your own
definitions instead of the system ones.

#### Define precedence operators with fixity declarations

* Keywords: `infixl | infixr | infix` for left/right/no associativity
* Syntax: `infix-keyword [0-9] operator [, operator]`
* Allowed wherever a type declaration is allowed
* 0 is lowest allowed fixity precedence, 9 is highest
* Prefix function application has fixity 10, higher than any infix call

Lambda abstractions, else clauses, and let...in clauses extend as far as
lexically possible, meaning they never stop at any infix operator, no
matter how low precedencer.

Default fixitity:

```
   infixl 9  !!             -- This is the default when fixity unspecified
   infixr 9  .
   infixr 8  ^, ^^, ⋆⋆
   infixl 7  ⋆, /, `quot`, `rem`, `div`, `mod`
   infixl 6  +, -           -- Unary negation "-" has this fixity, too
   infixr 5  ++             -- built-in ":" constructor has this fixity, too
   infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
   infixr 3  &&
   infixr 2  ||
   infixl 1  >>, >>=
   infixr 1  =<<
   infixr 0  $, $!, `seq`
```
