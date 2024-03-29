# Notes on Haskell 2010 Report

* A summary of the [Haskell 2010 Language Report][1]
* Do not know from where I downloaded the initial text version of this summary
* Converted to Markdown format
* A few clarifications

[1]: https://www.haskell.org/onlinereport/haskell2010/

## Part 1: Overview

### Top down structure (Gross overview)

* Modules - top most level
  * Provides way to control namespaces
  * Helps reuse software in large programs
* Module consists of a collection of declarations
  * Types of Declarations
    * ordinary values
    * datatypes
    * type classes
    * fixity information
  * Next level down are expressions
    * Has a value and a static type
    * Heart of Haskell programming "in the small"
  * Lowest Level is lexical structure
    * Captures the concrete representation of programs in text files

### Haskell Kernel (Factoids)

* Kernel not formally specified
  * Slightly sugared variant of the (typed) lambda calculus
  * Straightforward denotational semantics
  * Programs get reduced (desugared) to the kernel before symantics
  * Provides straight forward reasoning about programs
  * Provides useful guidelines for language implementation
  * Reflects true nature of the beast
* Language does not have statements (not imperative)
* Language is non-strict (lazy)
  * expressions evaluated only if necessary
* Data is immutable
  * Names are bound to expressions, not storage areas in memory.
  * Once bound, can not be changed
* Functions arguments separated by whitespace

   ```haskell
      myFunction arg1 arg2 arg3
   ```

* The Prelude adds complexity
  * This on top of the sugar added to the kernel
  * Kernel, Compiler, Prelude boundaries can be implementation dependent

### Haskell Namespaces

* Six kinds of names in Haskell
  * variables and Constructors denote values
  * Type system consists of
    * type variables
    * type constructors
    * type classes
  * Module names
* Two constraints on naming
  * Names of variables and type variables are identifiers beginning
    with lowercase letters or underscore.
  * Other types of identifiers begin with uppercase letters.
  * An identifier must not be used as the name of a type constructor and
    a type class in the same scope.
* Concrete naming example
  * Int may simultaneously be the name of a module, type class, and
    data constructor within a single scope.

## Part 2: Lexical Structure

### Notational conventions

```haskell
   `[pattern]`      optional
   `{pattern}`      zero or more repetitions
   `(pattern)`      grouping
   `pat1 ǀ pat2`    choice
   `pat<pat'>`      difference - elements generated by pat except those by pat'
```

### BNF-like syntax

### Haskell uses Unicode character set

* Infrastructure biased toward ASCII for historical reasons
* Compilers expected to make use of new versions of Unicode
* Syntax depends on how characters are defined by Unicode consortium
* This choice makes UNIX no longer completely Text/Binary agnostic, a small
  price tp pay for UTF-8 compatibility.

### Lexical Program Structure

* See page 8 of 2010 standard for gory details
* Lexical analysis uses "maximal munch" rule
  * Longest possible lexeme is read
    * `=` is reserved but `==` and `~=` are not
    * case is reserved but cases is not
  * Any kind of whitespace is a proper delimiter for lexemes
  * Only ANY type of characters are valid in Haskell programs
    * graphic character
    * whitespace character
  * `newline ➔ cr lf ǀ cr ǀ lf ǀ ff`
  * Comments are valid whitespace
    * Ordinary comments begin with `--` and extend to newline
    * The sequence of dashes must not form part of a legal lexeme
      * `-->` and `|--` don't begin comments
      * `--foo` does start a comment
      * `func "foo--bar"` does not start a comment
    * Nested commets start {- and end -}
      * No legal lexeme starts with {-
      * Can be nested to any depth
      * Nested comments used for compiler pragmas
      * Comments are not lexically analysed.  Any instance
        of `{-` or `-}` within a string or end-of-line comment
        will interfere with the nested comment

### Identifiers and Operators

```haskell
   identifier ➔ letter{letter ǀ digit ǀ _ ǀ '}
```

* Case sensitive, _ is treated as lower case
* Separate namespaces
  * variable identifiers start with lower case letter

     ```haskell
        varid ➔ (small {small ǀ large ǀ digit ǀ '})<reservedid>
     ```

  * constructor identifiers start with upper case letter

     ```haskell
        conid ➔ large {small ǀ large ǀ digit ǀ '}
     ```

### Reserved identifiers

```haskell
   reservedid ➔ case ǀ class ǀ data ǀ default ǀ deriving ǀ do ǀ else
              ǀ foreign ǀ if ǀ import ǀ in ǀ infix ǀ infixl ǀ infixr
              ǀ instance ǀ let ǀ module ǀ newtype ǀ of ǀ then
              ǀ type ǀ where ǀ _
```

## Operator symbols

```haskell
   special  ➔   | ǀ , ǀ ; ǀ [ ǀ ] ǀ ` ǀ { ǀ }
   symbol ➔ asciiSymbol ǀ unicodeSymbol<special ǀ _ ǀ " ǀ '>
   reservedop ➔ .. ǀ : ǀ :: ǀ = ǀ \ ǀ | ǀ <- ǀ -> ǀ @ ǀ ~ ǀ =>
   varsym ➔ (symbol<:> {symbol})<reservedop ǀ dashes>
   consym ➔ ( : {symbol})<reservedop>
```

* An operator symbol starting with a colon is a constructor,
  otherwise it is an ordinary identifier
* `:`, `[]`, and `[a,b]` list constructors are built into the
  base language (kernel???) to make more compatible
  with the LISP family of functional languages
* With the exception of the prefix operator `-`, all other operators
  are infix.  May need parentheses to force `-` prefix

  ```haskell
     ghci> 4 - (- 3)
     7
  ```

* Operators are just functions and can be called as such

  ```haskell
     ghci> 2 + 3
     5
     ghci> (+) 2 3
     5
  ```

* Functions have infix forms too

  ```haskell
     ghci> mod 8 3
     2
     ghci> 8 `mod` 3
     2
  ```

### The six different type of names

```haskell
   varid
   conid
   tyvar ➔ varid
   tycon ➔ conid
   tycls ➔ conid
   modid ➔ {conid .} conid
```

* Variables and type variables begin with small letters
  The others with capital letters.
  * aside: that is why True is capitalized
* Variables and constructors have infix forms, the others do not
* Variables, data constructors, type constructors, and type classes
  can have "qualified" names, but not type variables or module names

   ```haskell
      qvarid ➔ [modid .] varid
      qconid ➔ [modid .] conid
      qtycon ➔ [modid .] tycon
      qtycls ➔ [modid .] tycls
      qvarsym ➔ [modid .] varsym
      qconsym ➔ [modid .] consym
   ```

### Sample Lexical Analysis

* `f.g` lexes as 3 tokens

  ```haskell
     f . g
  ```

* `F.g` lexes as a qualified name

  ```haskell
     F.g
  ```

* `f..` lexes as 2 tokens

  ```haskell
     f ..
  ```

* `F..` lexes as a qualified `.`
* `F.`  lexes as 2 tokens

  ```haskell
     F .
  ```

* Prelude.+ is an infix operator with same fixity as + in the Prelude
* `bar(3)` lexes as 4 tokens

  ```haskell
     bar ( 3 )
  ```

  and eventually sematically interpretted as

  ```haskell
     bar 3
  ```

### Numeric Literals

```haskell
   decimal ➔ digit{digit}
   octal   ➔ octit{octic}
   hexadecimal ➔ hexit{hexit}

   integer ➔ decimal
           ǀ  0o octal ǀ 0O octal
           ǀ  0x hexadecimal ǀ 0X hexadecimal

   float ➔ decimal.decimal[exponent]
         ǀ  decimal[exponent]

   exponent ➔ (eǀE)[+ǀ-]decimal
```

* Floating point literals are always expressed with decimal notation.
* Digits before and after decimal point, no whitespace.
* Negative numbers may require some care.
* Typing of literals to be discussed later.

### Character and String Literals

```haskell
   char    ➔  ' (graphic<'ǀ\> ǀ space ǀ escape<\&>) '
   string  ➔  " {graphic<"ǀ\> ǀ space ǀ escape ǀ gap} "
   escape  ➔  \ (charesc ǀ ascii ǀ decimal ǀ o octal ǀ x hexadecimal)
   charesc ➔  a ǀ b ǀ f ǀ n ǀ r ǀ t ǀ v ǀ \ ǀ " ǀ ' ǀ &
   ascii   ➔  ^ cntrl ǀ NUL ǀ SOH ǀ STX ǀ ETX ǀ EOT ǀ ENQ ǀ ACK
           ǀ   BEL ǀ BS ǀ HT ǀ LF ǀ VT ǀ FF ǀ CR ǀ SO ǀ SI ǀ DLE
           ǀ   DC1 ǀ DC2 ǀ DC3 ǀ DC4 ǀ NAK ǀ SYN ǀ ETB ǀ CAN
           ǀ   EM ǀ SUB ǀ ESC ǀ FS ǀ GS ǀ RS ǀ US ǀ SP ǀ DEL
   cntrl   ➔  ascLarge ǀ @ ǀ [ ǀ \ ǀ ] ǀ ^ ǀ _
   gap     ➔  \ whitechar {whitechr} \
   whitechar ➔ newline ǀ vertab ǀ space ǀ tab ǀ uniWhite
```

* Think of `\&` as a zero width separator
  * `"foo\&bar" == "foobar"`
  * `"\SOH"`is one character
  * `"\SO\&H"`is two characters
  * `'\&'`is illegal

### Layout

* In the kernel language `{ ... }` is used for
  grouping and `;` for expression separation
  * Very rarely seen used in code
  * Sometimes useful in ghci one-liners.
  * Makes Haskell programs easier to be produced by other programs.
  * Makes C wonks feel more at home.
  * The `;` is used like in Pascal as a separator except
    for expressions, Haskell has no statements.
* Layout optional, gives Haskell more of a Python feel
  * Culture (parser too?) puts the `;` before the next
    statement instead of after the previous one.
  * Parser "substitutes" certain indentation with brackets and semicolens.
* Example with layout

  ```haskell
     size :: Stack a -> Int
     size s = length (stkToLst s) where
                stkToLst Empty          = []
                stkToLst (MkStack x s)  = x:xs where xs = stkToLst s
  ```

* How the parser would interpret above

  ```haskell
     size :: Stack a -> Int
     ;size s = length (stkToLst s) where
                {stkToLst Empty          = []
                ;stkToLst (MkStack x s)  = x:xs where {xs = stkToLst s
     }}
  ```

* How a C coder would hand code it, tastes vary

  ```haskell
     size :: Stack a -> Int;
     size s = length (stkToLst s) where {
                  stkToLst Empty  = [];
                  stkToLst (MkStack x s) = x:xs where {
                        xs = stkToLst s
                  }
     }
  ```

* In a more Haskellian style

  ```haskell
     size :: Stack a -> Int
     ; size s = length (stkToLst s) where
                { stkToLst Empty  = []
                ; stkToLst (MkStack x s) = x:xs where
                                    { xs = stkToLst s }
                }
  ```

* How to type above into ghci

```haskell
   ghci> let size s=length (s2L s) where {s2L Empty=[];s2L (MKStack x s)=x:xs where {xs=s2L s}}
```

  Sometimes I find this is easier than messing with `:{` and `:}` within ghci

* Layout and non-layout can be mixed

```haskell
   f x = let a = 1; b = 2
             g y = exp2
          in exp1
```

   this makes `a, b, g` all part of the same "layout list."

## Part 3: Expressions

### Introduction

* This part describes the syntax and informal syntax
  of Haskell expressions, including their translation
  into the Haskell kernel where appropriate.
* Except in the case of "let" expressions, these kernel
  translations preserve static and dynamic semantics.
* Free variables and constructors used always refer to
  entities defined in the Prelude and may not necessarily
  be in scope.

### Structure of expressions

```haskell
   exp      ➔  infixexp :: [context =>] type      (expression type signature)
            ǀ  infixexp

   infixexp ➔  lexp qop infixexp                  (infix operator application)
            ǀ  - infixexp                         (prefix negation)
            ǀ  lexp

   lexp     ➔  \ apat1 ... aptn -> exp            (lambda abstraction, n ≥ 1)
            ǀ  let decls in exp                   (let expression)
            ǀ  if exp [;] then exp [;] else exp   (conditional)
            ǀ  case exp of { alts }               (case expression)
            ǀ  do { stmts }                       (do expression)
            ǀ  fexp

   fexp     ➔  [fexp] aexp                        (function application)

   aexp     ➔  qvar                               (variable)
            ǀ  gcon                               (general constructor)
            ǀ  literal
            ǀ  ( exp )                            (parenthesized expression)
            ǀ  ( exp1, ... , expn )               (tuple, n ≥ 2)
            ǀ  [ exp1, ... , expn ]               (list, n ≥ 1)
            ǀ  [ exp1, [exp2, ] .. [exp3]         (arithmetic sequence)
            ǀ  [ exp | qual1, ..., qualn ]        (list comprehension, n ≥ 1)
            ǀ  ( infixexp qop )                   (left section)
            ǀ  ( qop<-> infixexp )                (left section)
            ǀ  qcon { fbind1, ..., fbindn }       (labeled construction, n ≥ 0)
            ǀ  aexp<qcon> { fbind1, ..., fbindn } (labeled update, n ≥ 1)
```

* Use parenthesis to override infix operator's fixity.
* Consecutive operators with the same precedence must
  both be right or left associative.
* Negation is the only prefix operator.
* No postfix operators
* Grammar ambiguous regarding the extent of lambda
  abstractions, let expressions, and conditionals.
* Ambiguity resolved with meta-rule that each of these
  constructs extends as far right as possible
  * Actually as far **lexiconically** as possible.
  * Don't take "right" too literally.

  ```haskell
      Examples:                     Parses as:
      f x + g y                     (f x) + (g y)
      - f x + y                     (- (f x)) + y
      let { ... } in x + y          let { ... } in (x + y)
      z + let {...} in x + y        z + (let {...} in (x+y))
      f x y :: Int                  (f x y) :: Int
      \ x -> a+b :: Int             \x -> ((a+b) :: Int)
  ```

### Errors

* Errors during expression evaluation, denoted by `⊥` (bottom), are
  indistinguishable by a Haskell program from non-termination.
* Since Haskell is a non-strict language (only evaluate function
  arguments if necessary), all Haskell types include `⊥`.
* When evaluated, errors cause immediate program termination.
* Prelude provides two such functions
  * `error     :: String -> a`
  * `undefined :: a`
* Very implementation dependent

### Variables, Constructors, Operators, and Literals

```haskell
   aexp  ➔   qvar                      (variable)
         ǀ   gcon                      (general constructor)
         ǀ   literal

   gcon  ➔   ()
         ǀ   []
         ǀ   (,{,})
         ǀ   qcon

   var   ➔   varid ǀ ( varsym )        (variable)
   qvar  ➔   qvarid ǀ ( qvarsym )      (qualified variable)
```
