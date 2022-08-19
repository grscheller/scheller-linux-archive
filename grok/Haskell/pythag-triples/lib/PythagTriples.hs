module PythagTriples ( printTriples
                     , pythagTriplesOrdered1
                     , pythagTriplesOrdered2
                     , pythagTriplesFast
                     , showTriple
                     , sortTriple
                     , Triple ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)

type Triple = (Int, Int, Int)

sortTriple :: Triple -> Triple
sortTriple triple =
  case triple of
    (a, b, c) | b < a -> (b, a, c)
    _                 -> triple

printTriples :: [Triple] -> IO()
printTriples = mapM_ (putStrLn . showTriple)

-- | Standard algorithm to generate Pythagorean Triples
--   Each (a, b, c) have no common factors.
--   a & c are always odd, b is always even 
pythagTriplesFast :: Int -> Int -> [Triple]
pythagTriplesFast start end =
  let n = end
      m = if start < 2
            then 2
            else start
  in
    [ (a, b, c) |
       k <- [m .. n]
     , let nstart = k `mod` 2 + 1
     , j <- [nstart, nstart+2 .. k-1]
     , let a = k*k - j*j
     , let b = 2*j*k
     , let c = k*k + j*j ]

-- | Generate ordered Pythagorean Triples first by a then b.
--   For each a, will find all corresponding b's and c'c 
--   before moving onto the next a.
pythagTriplesOrdered1:: Int -> Int -> [Triple]
pythagTriplesOrdered1 start end =
  let n = end
      m = if start < 3
            then 3
            else start
  in
    [ (a, b, c) |
       a <- [m .. n]
     , b <- [a+1, a+3 .. ((a*a - 1) `div` 2)]
     , gcd b a == 1
     , let csqr = a*a + b*b
     , isPerfectSquare csqr
     , let c = floorSqrt csqr ]

-- | Generate ordered Pythagorean Triples first by b then a
--   Will only find a's where a < b
pythagTriplesOrdered2 :: Int -> Int -> [Triple]
pythagTriplesOrdered2 start end = 
  let n = end
      m = if start < 4
            then 4
            else start
  in
    [ (a, b, c) |
        b <- [m .. n]
      , a <- [(floorSqrt $ 2*b + 1) .. b - 1]
      , gcd b a == 1
      , let csqr = a*a + b*b
      , isPerfectSquare csqr
      , let c = floorSqrt csqr ]

-- Utility functions

floorSqrt :: Int -> Int
floorSqrt = floor.sqrt.fromIntegral

isPerfectSquare :: Int -> Bool
isPerfectSquare n = let m = floorSqrt n in  n == m * m

-- | Print out Pythagorean Triples with space
--   after comma, like how Python prints tuples.
showTriple :: Triple -> String
showTriple = intercalate ", " . splitOn "," . show
