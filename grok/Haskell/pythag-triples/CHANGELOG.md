# Revision history for pythag-triples

## 0.4.0 -- 2020-04-14

* First version where I try and do things canonically correctly
* Previous packages were called pythagTriples
* "Released" as part of my scheller-linux-archive GitHub repo

## 0.4.2 -- 2022-08-10

* got whatever was here to rebuild with Stack
* commit 5f1115830d1965c252997c7a817f8d9ddfa9bde4
* Neovim LSP with haskell-language-server totally misconfigured

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
