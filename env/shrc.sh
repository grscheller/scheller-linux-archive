#!/bin/sh
# shellcheck shell=sh
# shellcheck source=/dev/null
##
#  ~/.shrc
#
#   Written by Geoffrey Scheller
#   See: https://github.com/grscheller/scheller-linux-archive/env
#

## If not interactive, don't do anything.
case $- in
    *i* ) : ;;
     *  ) return ;;
esac

## Shell configuration
HISTSIZE=5000
set -o vi

PS1='$ '

# Similar to the DOS path command
path () {
   if [ $# -eq 0 ]
   then
       PathWord="$PATH"
   else
       PathWord="$1"
   fi

   # shellcheck disable=SC2086
   ( IFS=':'; printf '%s\n' $PathWord )
}

alias digpath='$HOME/.local/bin/digpath.bash'
# alias digpath='$HOME/.local/bin/digpath.sh'

## Make sure other shells have their correct environments
alias bash='ENV= bash'
alias ksh='ENV=~/.kshrc ksh'

MyShell=${0#-}; MyShell=${MyShell##*/}
case "$MyShell"X in
  shX)
      alias dash='ENV=~/.shrc dash'
      alias ash='ENV=~/.shrc ash'
      ;;
  ashX)
      alias sh='ENV=~/.shrc sh'
      alias dash='ENV=~/.shrc dash'
      ;;
  dashX)
      alias sh='ENV=~/.shrc sh'
      alias ash='ENV=~/.shrc ash'
      ;;
esac
unset MyShell
