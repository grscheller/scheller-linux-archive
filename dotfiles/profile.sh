#!/bin/sh
# shellcheck shell=sh
# shellcheck source=/dev/null
##
#   ~/.profile
#
#   Written by Geoffrey Scheller
#   See: https://github.com/grscheller/scheller-linux-archive/env
#

## Configurature an initial interactive environment
[ -r ~/.envrc ] && . ~/.envrc

## If Bash, get functions and aliases
MyShell=${0#-}; MyShell=${MyShell##*/}
case "$MyShell"X in
  bashX)
      if [ -r ~/.bashrc ]; then
          . ~/.bashrc
      fi
      ;;
  kshX|shX|ashX|dashX)
      :
      ;;
  *)
      printf 'Warning: Unexpected shell "%s\n"' "$0" >&2
      ;;
esac
unset MyShell

## Perform other tasks unique to actual login shells
