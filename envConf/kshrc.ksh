#!/bin/ksh
# shellcheck shell=ksh
# shellcheck source=/dev/null
#
#  ~/.kshrc
#
# Korn Shell configuration across multiple, more or
# or less, POSIX complient systems.

## If not interactive, don't do anything.
[[ $- != *i* ]] && return

## Make sure an initial shell environment is well defined
#
#    Shells in terminal windows not necessarily
#    descendant from a login shell.
#
export ENV_INIT_LVL=${ENV_INIT_LVL:=0}
((ENV_INIT_LVL < 1)) && . ~/.env_init

## Read in aliases and functions
. ~/.envrc

## Make sure other shells have their environments
alias dash='ENV=~/.dashrc dash'
alias sh='ENV=~/.shrc sh'
alias bash='ENV= bash'
if ~/bin/digpath mksh > /dev/null 2>&1
then
    alias mksh='ENV=~/.kshrc mksh'
fi

## Modify 3 line prompt for KSH - end with  '$ '
PS1="${PS1%??}$ "

