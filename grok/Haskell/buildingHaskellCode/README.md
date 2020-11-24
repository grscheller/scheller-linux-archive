# Building Haskell Code with Cabal

I am finding that Cabal has advantages over Stack when dealing with
the locally installed Arch GHC.  Stack is more for freezing a build
consistently across different architectures.  Cabal seems to work well with
the "rolling distribution" paradigm.  When using Cabal, different projects,
using different interrelated sets of dependencies, need to be put into
different Cabal sandboxes to avoid "Cabal Hell."  Stack uses Cabal sandboxes
under-the-hood.

## 1. Build a Haskell program from commandline

Let us start with a completely trivial Haskell program,
[cmdLine/hw.hs](cmdLine/hw.hs),
with the following contents:

```
   module Main where

   main :: IO ()
   main = putStrLn("Hello, World!")
```

Since version 8.0.2-1, the Arch Linux ghc package no longer contains static
versions of the GHC boot libraries.  You have to explicitly install the
ghc-static package to get the static libraries and documentation.

I think the hint is to use dynamic linking whenever possible.  Use static
linking when you want to distribute code across "sufficiently" binary
compatible systems.

```
   $ cd cmdLine
   $ ghc -Wall -dynamic hw.hs
```

to run,

```
   $ ./hw
   Hello, World!
```

Lets see what was built,

```
   $ ls -l hw
   -rwxrwx--- 1 geoff geoff 17544 Mar 11 21:52 hw

   $ ldd hw
       linux-vdso.so.1 (0x00007fffe4dcd000)
       libm.so.6 => /usr/lib/libm.so.6 (0x00007f385f8aa000)
       libHSbase-4.10.1.0-ghc8.2.2.so =>
          /usr/lib/ghc-8.2.2/base-4.10.1.0/libHSbase-4.10.1.0-ghc8.2.2.so (0x00007f385eed9000)
       libHSinteger-gmp-1.0.1.0-ghc8.2.2.so =>
          /usr/lib/ghc-8.2.2/integer-gmp-1.0.1.0/libHSinteger-gmp-1.0.1.0-ghc8.2.2.so (0x00007f385fda7000)
       libHSghc-prim-0.5.1.1-ghc8.2.2.so =>
          /usr/lib/ghc-8.2.2/ghc-prim-0.5.1.1/libHSghc-prim-0.5.1.1-ghc8.2.2.so (0x00007f385ea39000)
       libHSrts-ghc8.2.2.so => /usr/lib/ghc-8.2.2/rts/libHSrts-ghc8.2.2.so (0x00007f385fd40000)
       libgmp.so.10 => /usr/lib/libgmp.so.10 (0x00007f385e7a6000)
       librt.so.1 => /usr/lib/librt.so.1 (0x00007f385e59e000)
       libdl.so.2 => /usr/lib/libdl.so.2 (0x00007f385e39a000)
       libffi.so.6 => /usr/lib/libffi.so.6 (0x00007f385e191000)
       libpthread.so.0 => /usr/lib/libpthread.so.0 (0x00007f385df73000)
       libc.so.6 => /usr/lib/libc.so.6 (0x00007f385dbbc000)
       /lib64/ld-linux-x86-64.so.2 => /usr/lib64/ld-linux-x86-64.so.2 (0x00007f385fbf6000)
```

Now, rebuild making the ghc libraries static.

```
   $ rm hw hw.hi hw.o
   $ ghc -Wall hw.hs
```

run it again,

```
   $ ./hw
   Hello, World!
```

this time, what was built is a bit bigger,

```
   $ ls -l hw
   -rwxrwx--- 1 geoff geoff 1079760 Mar 11 22:09 hw

   $ ldd hw
       linux-vdso.so.1 (0x00007ffd34741000)
       libm.so.6 => /usr/lib/libm.so.6 (0x00007f97dca20000)
       libgmp.so.10 => /usr/lib/libgmp.so.10 (0x00007f97dc78d000)
       librt.so.1 => /usr/lib/librt.so.1 (0x00007f97dc585000)
       libdl.so.2 => /usr/lib/libdl.so.2 (0x00007f97dc381000)
       libffi.so.6 => /usr/lib/libffi.so.6 (0x00007f97dc178000)
       libpthread.so.0 => /usr/lib/libpthread.so.0 (0x00007f97dbf5a000)
       libc.so.6 => /usr/lib/libc.so.6 (0x00007f97dbba3000)
       /lib64/ld-linux-x86-64.so.2 => /usr/lib64/ld-linux-x86-64.so.2 (0x00007f97dcd6c000)
```
