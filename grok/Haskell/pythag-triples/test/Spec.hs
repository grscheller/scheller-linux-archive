module Main (main) where

import Data.List
import PythagTriples

-- | Test suite for pythag-triples package,
--   not a proper test suite.
--     to run as a test:    "$ cabal test"
--     to run as a program: "$ cabal run pythag-triples"
main :: IO ()
main = test1 2
    >> test1 5
    >> test1 10
    >> test2 10
    >> test2 100
    >> test2 1000

test1 :: Int -> IO ()
test1 nn =
  putStrLn ("\nTest1: For " ++ "2^" ++ show nn ++ " (Ordered, Sorted Fast)")
    >> mapM_ print (zip py3Ordered py3SortedFast)
  where
    ordered = pythagTriplesOrdered1 3 $ 2 ^ nn
    sortedFast = sort $ map sortTriple $ pythagTriplesFast 2 $ 2 ^ nn
    lastEq = lastEqual ordered sortedFast
    py3Ordered = take 5 $ drop (lastEq - 2) ordered
    py3SortedFast = take 5 $ drop (lastEq - 2) sortedFast

test2 :: Int -> IO ()
test2 nn =
  putStrLn ("\nTest2: For " ++ show nn ++ "  Last equal = " ++ show lastEq)
    >> mapM_ print (zip py3Ordered py3SortedFast)
  where
    ordered = pythagTriplesOrdered1 3 nn
    sortedFast = sort $ map sortTriple $ pythagTriplesFast 2 nn
    lastEq = lastEqual ordered sortedFast
    py3Ordered = take 5 $ drop (lastEq - 2) ordered
    py3SortedFast = take 5 $ drop (lastEq - 2) sortedFast

sortTriples :: [Triple] -> [Triple]
sortTriples triples = sort $ map sortTriple triples

lastEqual :: [Triple] -> [Triple] -> Int
lastEqual trip1 trip2 =
  iterIt 0 trip1 trip2
  where
    iterIt :: Int -> [Triple] -> [Triple] -> Int
    iterIt n [] trip2 = n
    iterIt n trip1 [] = n
    iterIt n trip1 trip2 =
      if head trip1 == head trip2
        then iterIt (n + 1) (tail trip1) (tail trip2)
        else n
