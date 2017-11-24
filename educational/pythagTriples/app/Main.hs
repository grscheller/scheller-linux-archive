module Main where

import System.Environment (getArgs)
import Data.List (sort)
import PythagTriples ( pythagTriplesOrdered
                     , pythagTriplesFast
                     , sortTriple
                     , Triple
                     , printTriples )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-o", numStr]  -> printTriples $ triplesOrdered (read numStr)
    ["-f", numStr]  -> printTriples $ triplesFast (read numStr)
    ["-fs", numStr] -> printTriples $ sort $ map sortTriple $ triplesFast (read numStr)
    ["-h"]          -> putStrLn $ usageString ++ infoString
    ('-':_):_       -> error("\n\n  Error: Called with an invalid option or wrong number of arguments.\n" ++ usageString)
    [numStr]        -> printTriples $ triplesFast (read numStr)
    _               -> error("\n\n  Error: Called with invalid arguments.\n" ++ usageString)


triplesOrdered :: Int -> [Triple]
triplesOrdered num = take num pythagTriplesOrdered

triplesFast :: Int -> [Triple]
triplesFast num = take num pythagTriplesFast

usageString :: String
usageString = unlines [
    "  "
  , "  Usage: pythagTriples  [-o|-f|-fs|-h] number"
  , "    where"
  , "      number = number of triples to print"
  , "    and"
  , "      -o  Triples (a, b, c) are printed in lexiconical order,"
  , "          that is a < b < c, where a,b,c have no common factors."
  , "          Algorithm prints all possible b's and c's before"
  , "          going onto the next a."
  , "      -f  Use a fast algorithm where triples (a, b, c) are such"
  , "          that a is odd, b is even, and a,b,c have no common"
  , "          factors."
  , "      -fs Use the fast algorithm, but sort results a < b < c."
  , "      -h  Print usage and general information."
  ]

infoString :: String
infoString = unlines [
    "    "
  , "    Pathagorean triples are integers 0 < a, b, c such that"
  , "    a^2 + b^2 = c^2.  Both algorithms only print triples"
  , "    with no common factors, that is  gcd(a,b,c) = 1."
  ]

