module Main where

import           Data.List          (sort)
import           PythagTriples      (printTriples, pythagTriplesFast,
                                     pythagTriplesOrdered1,
                                     pythagTriplesOrdered2, sortTriple)
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["-o1", start, end] -> printTriples $
                               pythagTriplesOrdered1 (read start) (read end)
      ["-o2", start, end] -> printTriples $
                               pythagTriplesOrdered2 (read start) (read end)
      ["-f", start, end]  -> printTriples $
                               pythagTriplesFast (read start) (read end)
      ["-fs", start, end] -> printTriples $ sort $ map sortTriple $
                               pythagTriplesFast (read start) (read end)
      ["-o1", end]        -> printTriples $
                               pythagTriplesOrdered1 3 (read end)
      ["-o2", end]        -> printTriples $
                               pythagTriplesOrdered2 4 (read end)
      ["-f", end]         -> printTriples $
                               pythagTriplesFast 2 (read end)
      ["-fs", end]        -> printTriples $ sort $ map sortTriple $
                               pythagTriplesFast 2 (read end)
      "-h": _             -> putStrLn $ usageString ++ infoString
      "--help":_          -> putStrLn $ usageString ++ infoString
      "-o1":_             -> errorOut "option -o1 takes one or two arguments"
      "-o2":_             -> errorOut "option -o2 takes one or two arguments"
      "-fs":_             -> errorOut "option -fs takes one or two arguments"
      "-f":_              -> errorOut "option -f takes one or two arguments"
      ('-':x:rest):_      -> errorOut $ '-':x:rest ++ " is an invalid option"
      [start, end]        -> printTriples $ sort $ map sortTriple $
                               pythagTriplesFast (read start) (read end)
      [end]               -> printTriples $ sort $ map sortTriple $
                               pythagTriplesFast 2 (read end)
      []                  -> errorOut "called with no arguments"
      _                   -> errorOut "called with wrong number of arguments"
  where
    errorOut str = error $
      "\n  error, PythagTriples: " ++ str ++
      ",\n  for help, try 'PythagTriples --help'\n"

usageString :: String
usageString =
  unlines
    [ "  "
    , "  Usage: pythagTriples [-o1|-o2|-f|-fs|-h|--help] [start] end"
    , "    where"
    , "      -o1 Triples (a, b, c) are generated in lexiconical order,"
    , "          that is a < b < c, where a,b,c have no common factors,"
    , "          starting with a=3 (or start) and ending with a=end."
    , "          Algorithm prints all possible b's and c's before"
    , "          going onto the next a."
    , "      -o2 Triples (a, b, c) are generated ordered first by b"
    , "          then by a.  For each b, all a < b are generated,"
    , "          starting with b=4 (or start) and endding with b=end."
    , "      -f  Use a fast algorithm where triples (a, b, c) are such"
    , "          that a is odd, b is even, and a,b,c have no common"
    , "          factors.  Both start and end signify iteration levels"
    , "          used by the algorithm."
    , "      -fs Use above fast algorithm, sort results a < b < c."
    , "          This is the default when no options are given."
    , "      -h  Print usage and general information."
    ]

infoString :: String
infoString =
  unlines
    [ "    "
    , "    Pathagorean triples are integers 0 < a, b, c such that"
    , "    a^2 + b^2 = c^2.  These algorithms only print triples"
    , "    with no common factors, that is gcd(a,b,c) = 1."
    ]
