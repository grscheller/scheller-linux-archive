# Revision history for pythag-triples x.y.z

Initially, I set up the project using sloppy 3 digit versioning.

## 0.4.0 -- 2020-04-14

* First version where I try and do things canonically correctly
* Previous packages were called pythagTriples
* "Released" as part of my scheller-linux-archive GitHub repo

## 0.4.2 -- 2022-08-10

* got whatever was here to rebuild with Stack
* commit 5f1115830d1965c252997c7a817f8d9ddfa9bde4
* Neovim LSP with haskell-language-server totally misconfigured

# Revision history for pythag-triples w.x.y.z

As of version 0.5.4.0, I am using strict 4 digit versioning.

```
   https://pvp.haskell.org
   PVP summary:      +--------- epic events/direction changes
                     | +------- breaking API changes
                     | | +----- non-breaking API additions
                     | | | +--- code changes with no API change
   version:          0.1.0.0
```

## 0.5.0.0 -- 2022-08-15

* rebuild using just native Arch Haskell toolchain - dynamic linking
* use cabal from the cabal-install Pacman package as build tool

  1. Move original code aside
  2. Generate boilerplate: `$ cabal init`
  3. Test boilerplate code: `$ cabal run`

## 0.5.1.0 -- 2022-08-16

* first version where original code incorporated back into executable
* library code and executable combinded into single executable
* intend to separate out library at a later time

## 0.5.1.1 -- 2022-08-16

* incorporated changes from neovim lsp haskell-language-server feedback
* haddock changes

## 0.5.2.1 -- 2022-08-17

* beginning internal changes
* `pythagTriplesOrdered2 :: Int -> Int -> [Triple]`
* `pythagTriplesOrdered2 start stop`
* `$ pythagTriples -o2 stop` prints out Triples (a, b, c) for b <= stop
skell 
## 0.5.2.2 -- 2022-08-18

* `$ pythagTriples [-f[s]] n` changed

  1. no longer just doing a `take n` on an infinite data structure
  2. n signifies a level interation
  3. seems that the gcd check was superfluous 

* got rid of some helper functions

## 0.5.3.0 -- 2022-08-18

* `$ pythagTriples` now handles 1 or 2 nonoption arguments
* for -o1 or -o2 you can now give both a start and end arguments
* for -f or -fs you can give start and end "iteration level" arguments

## 0.5.3.1 -- 2022-08-19

* preparing to split out pythag-triples library to lib/
* tweak pythag-triples.cabal
* improved pythagTriples error messages

## 0.5.4.0 -- 2022-08-19

* split out pythag-triples library code from pythagTriples to lib/

## 0.5.4.1 -- 2022-08-20

* added back test code from pre 0.4 versions
* not yet a proper test suite
  * to run the test suite: `$ cabal test`
  * to run test suite as a program: `$ cabal run pythag-triples-test`
                          
## 0.5.4.2 -- 2022-08-20

* added `gcd j k == 1` check to pythagTriplesFast function

## 0.5.4.3 -- 2022-08-22

* should be no changes in behavior, just playing around with Haskell linters

## 0.5.4.4 -- 2022-08-23

* practicing editing Haskell with Neovim lsp and haskell-language-server
  * mostly formatting changes to reflect readability of code
  * more consistent naming of variables 

## 0.6.0.0 -- 2022-08-23

* fast algorithm now prints out in a more partially ordered way
  * more consistent with -o1
