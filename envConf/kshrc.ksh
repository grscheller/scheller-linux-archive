#!/bin/ksh
# shellcheck shell=ksh
# shellcheck source=/dev/null
#
#  ~/.kshrc
#
# Korn Shell configuration across multiple, more or
# or less, POSIX complient systems.

export ENV_INIT_LVL=${ENV_INIT_LVL:=0}

## If not interactive, don't do anything.
[[ $- != *i* ]] && return

## Make sure an initial shell environment well defined
#
#    Shells in terminal windows not necessarily
#    descendant from a login shell.
#
if ((ENV_INIT_LVL < 1)) || ((DOT_PROFILE_SOURCED == 1)) 
then 
    . ~/.env_init
    unset DOT_PROFILE_SOURCED 
fi

## Read in aliases and functions
. ~/.envrc

## Make sure other shells have their environments
alias dash='ENV=~/.dashrc dash'
alias sh='ENV=~/.shrc sh --posix'

## Should already be in environment
export ENV=~/.kshrc

## Modify 3 line prompt for KSH - end with  '$ '
PS1="${PS1%??}$ "

