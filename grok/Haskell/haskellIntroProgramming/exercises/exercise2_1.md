# Exercise 2.1

## Evaluate `sumf (sumf square) [[1,2],[3,4]]`

Lets first desugar the expression.  This is done at compile time.

```haskell
sumf (sumf square) [[1,2],[3,4]]
sumf (sumf square) ([1,2]):([3,4]):[]    -- Outer desugaring
sumf (sumf square) (1:2:[]):(3:4:[]):[]  -- Inner desugaring
```

Now, the run time evaluation below assumes that:

* (+) is left associative.
* Functions are not strict, their argument only evaluated if needed.
* Haskell is lazy, nothing is evaluated until needed/driven.
* All expressions are referentially transparent.
* Calculation is single threaded.
* Trivial steps, like `square 2 => 2 * 2 => 4`, are consolidated.

```haskell
sumf (sumf square) (1:2:[]):(3:4:[]):[]
(sumf square) (1:2:[]) + sumf (sumf square) (3:4:[]):[]
square 1 + sumf square 2:[] + sumf (sumf square) (3:4:[]):[]
1 + sumf square 2:[] + sumf (sumf square) (3:4:[]):[]
1 + square 2 + sumf square [] + sumf (sumf square) (3:4:[]):[]
1 + 4 + sumf square [] + sumf (sumf square) (3:4:[]):[]
5 + sumf square [] + sumf (sumf square) (3:4:[]):[]
5 + 0 + sumf (sumf square) (3:4:[]):[]
5 + sumf (sumf square) (3:4:[]):[]
5 + (sumf square) (3:4:[]) + sumf square []
5 + square 3 + (sumf square) (4:[]) + sumf square []
5 + 9 + (sumf square) (4:[]) + sumf square []
14 + (sumf square) (4:[]) + sumf square []
14 + square 4 + (sumf square []) + sumf square []
14 + 16 + (sumf square []) + sumf square []
30 + (sumf square []) + sumf square []
30 + 0 + sumf square []
30 + sumf square []
30 + 0
30
```
