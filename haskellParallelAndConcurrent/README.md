# Haskell Parallel & Concurrent Programming
Example code bits inspired by the book
[Haskell Parallel & Concurrent Programming in Haskell](http://shop.oreilly.com/product/0636920026365.do)
by Simon Marlow.

I started this project when I realized that I have not coded in Haskell
for so long, that I could not program in it, compile it, or use the Stack
toolchain off the top of my head anymore.  The parallel programing I have been
doing in Scala has made Simon Marlow's book much more accessible to me.

## 1. First step, using ghc from the command line.
Create a new file, taken from the beginning of chapter 7,
[firstConcurrentSteps/fork.hs](firstConcurrentSteps/fork.hs)
with the following contents:
```
import Control.Concurrent
import Control.Monad
import System.IO

   main = do
     hSetBuffering stdout NoBuffering
     forkIO (replicateM_ 20 (putChar 'A'))
     replicateM_ 20 (putChar 'B')
```
Since version 8.0.2-1, the Arch Linux ghc package no longer contains static
versions of the GHC boot libraries.  Therefore, we must include the `-dynamic`
flag when compiling.
```
   $ ghc -dynamic fork.hs
   [1 of 1] Compiling Main             ( fork.hs, fork.o )
   Linking fork ...
``` 
To run,
```
   $ ./fork 
   BBBBBABABABABABABABABABABABABABABABAAAAA
```

## 2. Get the stack tool chain working again.
Todo.
