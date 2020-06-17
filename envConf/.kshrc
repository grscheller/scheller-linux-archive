#  ~/.kshrc
#
# Korn Shell configuration across multiple, more or
# or less, POSIX complient systems.
#
# shellcheck shell=ksh
# shellcheck source=/dev/null

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
    source ~/.env_init
    unset DOT_PROFILE_SOURCED 
fi

## Read in aliases and functions
source ~/.envrc

## Modify 3 line prompt for KSH - end with  '$ '
PS1="${PS1%\> }$ "

