module PythagTriples
  ( printTriples
  , pythagTriplesOrdered1
  , pythagTriplesOrdered2
  , pythagTriplesFast
  , showTriple
  , sortTriple
  , Triple
  ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)

type Triple = (Int, Int, Int)

sortTriple :: Triple -> Triple
sortTriple triple =
  case triple of
    (a, b, c) | b < a -> (b, a, c)
    _                 -> triple

printTriples :: [Triple] -> IO()
printTriples triples = mapM_ putStrLn (map showTriple triples)

-- | Standard algorithm to generate Pythagorean Triples
pythagTriplesFast :: [Triple]
pythagTriplesFast = [ (a, 2*m*n, c) |
    m <- [2 ..]
  , let nstart = m `mod` 2 + 1
  , n <- [nstart, nstart+2 .. m-1]
  , let a = m*m - n*n
  , let c = m*m + n*n
  , gcd a c == 1 ]

-- | Generate ordered Pythagorean Triples lexiconically ordered
pythagTriplesOrdered1:: [Triple]
pythagTriplesOrdered1 = [ (a, b, c) |
      a <- [3 .. ]
    , b <- [a+1, a+3 .. ((a*a - 1) `div` 2)]
    , gcd b a == 1
    , let csqr = a*a + b*b
    , isPerfectSquare csqr
    , let c = floorSqrt csqr ]

-- | Generate ordered Pythagorean Triples lexiconically ordered
pythagTriplesOrdered2 :: [Triple]
pythagTriplesOrdered2 = [ (a, b, c) |
      b <- [4 .. ]
    , a <- [(floorSqrt $ 2*b + 1) .. b - 1]
    , gcd b a == 1
    , let csqr = a*a + b*b
    , isPerfectSquare csqr
    , let c = floorSqrt csqr ]

-- Utility functions

floorSqrt :: Int -> Int
floorSqrt = floor.sqrt.fromIntegral

isPerfectSquare :: Int -> Bool
isPerfectSquare = \n ->
    let m = floorSqrt n
    in  n == m * m

-- | Print out Pythagorean Triples with space
-- | after comma, like how Python prints tuples.
showTriple :: Triple -> String
showTriple = (intercalate ", ").(splitOn ",").show
