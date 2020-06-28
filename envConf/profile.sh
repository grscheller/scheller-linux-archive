#!/bin/sh
# shellcheck shell=sh
# shellcheck source=/dev/null
#
#   ~/.profile
#

## Configurature an common initial interactive environment
[ -r ~/.env_init ] && . ~/.env_init

## Get functions and aliases if ksh or bash
MyShell=${0#-}; MyShell=${MyShell##*/}

case "$MyShell"X in
  bashX)
      if [ -r ~/.bashrc ]; then
          . ~/.bashrc
      fi
      ;;
  kshX)
      if [ -r ~/.kshrc ]; then
          export ENV=~/.kshrc
      fi
      ;;
  mkshX)
      if [ -r ~/.mkshrc ]; then
          export ENV=~/.mkshrc
      elif [ -r ~/.kshrc ]; then
          export ENV=~/.kshrc
      fi
      ;;
  shX)
      if [ -r ~/.shrc ]; then
          export ENV=~/.shrc
      fi
      ;;
  dashX)
      if [ -r ~/.dashrc ]; then
          export ENV=~/.dashrc
      fi
      ;;
  *)
      printf 'Warning: Unexpected shell "%s\n"' "$0"
      ;;
esac

## Perform other tasks unique to actual login shells

