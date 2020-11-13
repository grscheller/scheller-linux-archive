#!/bin/sh
# shellcheck shell=sh
# shellcheck source=/dev/null
##
#   ~/.profile
#

## Configurature an initial interactive environment
#
[ -r ~/.envrc ] && . ~/.envrc

## If Bash, get functions and aliases
#
MyShell=${0#-}; MyShell=${MyShell##*/}
case "$MyShell"X in
  bashX)
      if [ -r ~/.bashrc ]; then
          . ~/.bashrc
      fi
      ;;
  kshX|mkshX|shX|dashX)
      :
      ;;
  *)
      printf 'Warning: Unexpected shell "%s\n"' "$0"
      ;;
esac

## Perform other tasks unique to actual login shells
#
