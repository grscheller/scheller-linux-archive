module Main (main) where

import System.Environment (getArgs)
import Data.List (sort)
import PythagTriples ( pythagTriplesOrdered1
                     , pythagTriplesOrdered2
                     , pythagTriplesFast
                     , sortTriple
                     , Triple
                     , printTriples )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-o1", numStr] -> printTriples $ triplesOrdered1 (read numStr)
    ["-o2", numStr] -> printTriples $ triplesOrdered2 (read numStr)
    ["-f", numStr]  -> printTriples $ triplesFast (read numStr)
    ["-fs", numStr] -> printTriples $ sort $ map sortTriple $ triplesFast (read numStr)
    "-h":_          -> putStrLn $ usageString ++ infoString
    "--help":_      -> putStrLn $ usageString ++ infoString
    "-o1":_         -> errorOut "option -o1 takes one argument"
    "-o2":_         -> errorOut "option -o2 takes one argument"
    "-f":_          -> errorOut "option -f takes one argument"
    "-fs":_         -> errorOut "option -fs takes one argument"
    ('-':x:rest):_  -> errorOut $ '-':x:rest ++ " is an invalid option"
    [numStr]        -> printTriples $ triplesFast (read numStr)
    _               -> errorOut "called with invalid arguments"
  where
    errorOut str =  error $ "\n  error: " ++ str

triplesOrdered1 :: Int -> [Triple]
triplesOrdered1 num = take num pythagTriplesOrdered1

triplesOrdered2 :: Int -> [Triple]
triplesOrdered2 num = take num pythagTriplesOrdered2

triplesFast :: Int -> [Triple]
triplesFast num = take num pythagTriplesFast

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
