#!/bin/sh
# shellcheck shell=sh
# shellcheck source=/dev/null
##
#  ~/.shrc
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

alias digpath='$HOME/bin/digpath.bash'
# alias digpath='$HOME/bin/digpath.sh'

## Make sure other shells have their environments
alias dash='ENV=~/.dashrc dash'
alias bash='ENV= bash'
if digpath -q ksh
then
    alias ksh='ENV=~/.kshrc ksh'
    if digpath -q mksh
    then
        alias mksh='ENV=~/.kshrc mksh'
    fi
elif digpath -q mksh
then
    alias ksh='ENV=~/.kshrc mksh'
fi
