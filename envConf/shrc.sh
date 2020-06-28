#!/bin/sh
# shellcheck shell=sh
# shellcheck source=/dev/null
#
#  ~/.dashrc
#

## If not interactive, don't do anything.
case $- in
    *i* ) : ;;
     *  ) return ;;
esac

HISTSIZE=5000
set -o vi

PS1='$ '

## Make sure other shells have their environments
alias sh='ENV=~/.shrc sh'
alias dash='ENV=~/.dashrc dash'
if ~/bin/digpath ksh > /dev/null 2>&1
then
    alias ksh='ENV=~/.kshrc ksh'
    if ~/bin/digpath mksh > /dev/null 2>&1
    then
        alias mksh='ENV=~/.kshrc mksh'
    fi
elif ~/bin/digpath mksh > /dev/null 2>&1
then
    alias ksh='ENV=~/.kshrc mksh'
fi

