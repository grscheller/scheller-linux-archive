-- Convert a file's line separators to the default system one.
--   First Argument is input file with unknown file ending.
--   Second argument is a copy of first file but with the
--   standard system line endings.

import System.Environment (getArgs)

interAct :: (String -> String) -> FilePath -> FilePath -> IO ()
interAct func inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (func input)

main :: IO ()
main = parseArgs
  where parseArgs = do
          args <- getArgs
          case args of
            [inFile, outFile] -> interAct myFunc inFile outFile
            _                 -> error("Exactly 2 file arguments needed")

myFunc = unlines . splitLines
             
--splitLines :: string -> [string]
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

