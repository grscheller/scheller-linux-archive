#  ~/.bashrc
#
# Bash configuration across multiple, more or
# or less, POSIX complient systems.
#
# shellcheck shell=bash
# shellcheck source=/dev/null

export ENV_INIT_LVL=${ENV_INIT_LVL:=0}

## If not interactive, don't do anything.
[[ $- != *i* ]] && return

# Debian and Arch Linux derived systems typically
# compile bash with option -DSYS_BASHRC which causes
# bash to source /etc/bash.bashrc before ~/.bashrc.
#
# Mechanism used on Redhat & Redhat derived systems
[[ -f /etc/bashrc ]] && . /etc/bashrc

# Note: Cygwin/MinGW/MYSYS2 environment startup scripts
#       are broken, without this bash completion only works
#       in a top level shell.
if [[ $(type -t __parse_options) != function ]]
then
    if [[ -f /usr/share/bash-completion/bash_completion ]]; then
        . /usr/share/bash-completion/bash_completion 
    elif [[ -f /usr/share/bash-completion/bash_completion.sh ]]; then
        . /usr/share/bash-completion/bash_completion.sh
    elif [[ -f /etc/bash_completion ]]; then
        . /etc/bash_completion
    elif [[ -f /etc/bash_completion.sh ]]; then
        . /etc/bash_completion.sh
    fi
fi

## Make BASH more Korn Shell like
shopt -s extglob
shopt -s checkwinsize
shopt -s checkhash
shopt -s cmdhist
shopt -s lithist
shopt -s histappend
PROMPT_COMMAND='history -a'
HISTFILESIZE=5000

## Make sure an initial shell environment is well defined
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

## Modify 3 line prompt for Bash - end with  '% '
PS1="${PS1%??}% "

## Make sure ksh has its environment
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

## Bash completion for stack (Haskell)
if ~/bin/digpath stack > /dev/null 2>&1
then
    eval "$(stack --bash-completion-script stack)"
fi

## Configure Anaconda3 Python Distribution
if [[ -d ~/opt/anaconda3 ]]
then
    # Have not tested out under ksh
    . ~/opt/anaconda3/etc/profile.d/conda.sh
fi
