module Main where

import System.Environment (getArgs)
import Data.List (sort)
import PythagTriples ( pythagTriplesOrdered
                     , pythagTriplesFast
                     , sortTriple
                     , Triple )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-o", numStr]  -> printTriples $ triplesOrdered (read numStr)
    ["-f", numStr]  -> printTriples $ triplesFast (read numStr)
    ["-fs", numStr] -> printTriples $ sort $ map sortTriple $ triplesFast (read numStr)
    ["-h"]          -> putStrLn usageString
    [numStr]        -> printTriples $ triplesFast (read numStr)
    _               -> error("\n  Error: Called with incorrect arguments.\n" ++ usageString)

printTriples :: [Triple] -> IO()
printTriples [] = return ()
printTriples (triple:triples) = 
  putStrLn (show triple) >> printTriples triples

triplesOrdered :: Int -> [Triple]
triplesOrdered num = take num pythagTriplesOrdered

triplesFast :: Int -> [Triple]
triplesFast num = take num pythagTriplesFast

usageString :: String
usageString = unlines [
  "  Usage: pythagTriples [-o|-f|-h] number",
  "    where",
  "      number = number of triples to print",
  "    and",
  "      -o  Triples (a, b, c) are printed in lexiconical order,",
  "          that is a < b < c, and a,b,c have no common factors.",
  "          Algorithm does prints all possible b's and c's before",
  "          going onto next a.",
  "      -f  Use fast algorithm where triples (a, b, c) are such",
  "          that a is odd, b is even, and a,b,c have no common",
  "          factors.",
  "      -fs Use fast algorithm, but sort results a < b < c.",
  "      -h  Print this usage message.",
  "   ",
  "    Pathagorean triples are integers 0 < a, b, c such that",
  "    a^2 + b^2 = c^2.  Both algorithms only print triples",
  "    with no common factors, that is  gcd(a,b,c) = 1." ]

