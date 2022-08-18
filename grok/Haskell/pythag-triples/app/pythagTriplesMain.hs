module Main where

import System.Environment (getArgs)
import Data.List (sort, intercalate)
import Data.List.Split (splitOn)

-- Part which may be turn into a Library

--import Data.List (intercalate)
--import Data.List.Split (splitOn)

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
pythagTriplesFast start stop =
  let n = stop
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
pythagTriplesOrdered1 start stop =
  let n = stop
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
pythagTriplesOrdered2 start stop = 
  let n = stop
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

-- Part to remain an executable

--import System.Environment (getArgs)
--import Data.List (sort)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-o1", numStr] -> printTriples $ pythagTriplesOrdered1 3 (read numStr)
    ["-o2", numStr] -> printTriples $ pythagTriplesOrdered2 4 (read numStr)
    ["-f", numStr]  -> printTriples $ pythagTriplesFast 2 (read numStr)
    ["-fs", numStr] -> printTriples $ sort $ map sortTriple $ pythagTriplesFast 2 (read numStr)
    "-h":_          -> putStrLn $ usageString ++ infoString
    "--help":_      -> putStrLn $ usageString ++ infoString
    "-o1":_         -> errorOut "option -o1 takes one argument"
    "-o2":_         -> errorOut "option -o2 takes one argument"
    "-f":_          -> errorOut "option -f takes one argument"
    "-fs":_         -> errorOut "option -fs takes one argument"
    ('-':x:rest):_  -> errorOut $ '-':x:rest ++ " is an invalid option"
    [numStr]        -> printTriples $ pythagTriplesFast 2 (read numStr)
    _               -> errorOut "called with invalid arguments"
  where
    errorOut str =  error $ "\n  error: " ++ str

usageString :: String
usageString = unlines [
    "  "
  , "  Usage: pythagTriples  [-o1|-o2|-f|-fs|-h] number"
  , "    where"
  , "      number = number of triples to print"
  , "    and"
  , "      -o1 Triples (a, b, c) are generated in lexiconical order,"
  , "          that is a < b < c, where a,b,c have no common factors."
  , "          Algorithm prints all possible b's and c's before"
  , "          going onto the next a."
  , "      -o2 Triples (a, b, c) are generated ordered first by b"
  , "          then by a.  For each b, all a < b are generated"
  , "      -f  Use a fast algorithm where triples (a, b, c) are such"
  , "          that a is odd, b is even, and a,b,c have no common"
  , "          factors."
  , "      -fs Use above fast algorithm, sort results a < b < c."
  , "      -h  Print usage and general information."
  ]

infoString :: String
infoString = unlines [
    "    "
  , "    Pathagorean triples are integers 0 < a, b, c such that"
  , "    a^2 + b^2 = c^2.  These algorithms only print triples"
  , "    with no common factors, that is  gcd(a,b,c) = 1."
  ]
