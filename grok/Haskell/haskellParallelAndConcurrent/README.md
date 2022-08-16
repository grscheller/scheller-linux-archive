# Haskell Parallel & Concurrent Programming

Example code bits inspired by the book
[Haskell Parallel & Concurrent Programming in Haskell](http://shop.oreilly.com/product/0636920026365.do)
by Simon Marlow.

## 1. First step, using ghc from the command line

Created a new file, taken from the beginning of chapter 7,
[firstConcurrentSteps/fork.hs](firstConcurrentSteps/fork.hs)
with the following contents:

```
import Control.Concurrent
import Control.Monad
import System.IO

   main :: IO ()
   main = do
     hSetBuffering stdout NoBuffering
     _ <- forkIO (replicateM_ 20 (putChar 'A'))
     replicateM_ 20 (putChar 'B')
```

To build,

```
   $ ghc -Wall -dynamic fork.hs
   [1 of 1] Compiling Main             ( fork.hs, fork.o )
   Linking fork ...
```

to run,

```
   $ ./fork
   BBBBBABABABABABABABABABABABABABABABAAAAA
```

## 2. Print out info on GHC's installation directories

[ghcInfo/ghcInfo.hs](ghcInfo/ghcInfo.hs)

I have locally installed Simon Marlow's ghc.paths cabal package via
the Arch Linux haskell-ghc-paths pacman package.  If I understand the
source code correctly, it reverse engineers the GHC build system and
makes available locations of GHC build system components via the Haskell
GHC.Paths module.

Seems that haskell-ghc-paths Pacman package is a prerequisite of the
haskell-language-server package.
