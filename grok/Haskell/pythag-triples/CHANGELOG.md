# Revision history for pythag-triples x.y.z

## 0.4.0 -- 2020-04-14

* First version where I try and do things canonically correctly
* Previous packages were called pythagTriples
* "Released" as part of my scheller-linux-archive GitHub repo

## 0.4.2 -- 2022-08-10

* got whatever was here to rebuild with Stack
* commit 5f1115830d1965c252997c7a817f8d9ddfa9bde4
* Neovim LSP with haskell-language-server totally misconfigured

# Revision history for pythag-triples 0.x.y.z

Since first digit is zero, I am ignoring it and effectively using
3 digit versioning.  Will go to full w.x.y.z when w >=1.

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
