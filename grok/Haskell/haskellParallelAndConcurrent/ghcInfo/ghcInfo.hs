-- ghcInfo.hs

-- | Prints out information about GHC's installation directories.

module Main where
import GHC.Paths

ghcLoc, libLoc, pkgLoc, docLoc :: FilePath
ghcLoc = GHC.Paths.ghc
libLoc = GHC.Paths.libdir
pkgLoc = GHC.Paths.ghc_pkg
docLoc = GHC.Paths.docdir

main :: IO ()
main = do
  putStrLn("ghc location:     " ++ ghcLoc)
  putStrLn("ghc lib location: " ++ libLoc)
  putStrLn("ghc pkg location: " ++ pkgLoc)
  putStrLn("ghc doc location: " ++ docLoc)
