#   ~/.bashrc
#
# Bash configuration across multiple, more or
# or less, POSIX complient systems.
#
# Reads in shell functions and aliases from ~/.kshrc
#
# shellcheck shell=bash
# shellcheck source=/dev/null

## If not interactive, don't do anything.
[[ $- != *i* ]] && return

## System wide configurations
#
# Mechanism used on Redhat & Redhat derived systems
[[ -f /etc/bashrc ]] && source /etc/bashrc
#
# Debian and Arch Linux derived systems typically
# compile bash with option -DSYS_BASHRC which causes
# bash to source /etc/bash.bashrc before ~/.bashrc.
#
# Reload Bash completion scripts if not already done above.
# Note: Cygwin/MinGW/MYSYS2 environment startup scripts
#       are broken, without this bash completion only works
#       in a top level shell.
if [[ $(type -t __parse_options) != function ]]
then
    if [[ -f /usr/share/bash-completion/bash_completion ]]; then
        source /usr/share/bash-completion/bash_completion 
    elif [[ -f /usr/share/bash-completion/bash_completion.sh ]]; then
        source /usr/share/bash-completion/bash_completion.sh
    elif [[ -f /etc/bash_completion ]]; then
        source /etc/bash_completion
    elif [[ -f /etc/bash_completion.sh ]]; then
        source /etc/bash_completion.sh
    fi
fi

## Make BASH more KSH like
shopt -s extglob  # Turn on extended pattern matching.
shopt -s checkwinsize
shopt -s checkhash
shopt -s cmdhist   # Store multiline commands as single entry
shopt -s lithist   # in history with embedded whitespace.
shopt -s histappend  # Append, don't replace history file.
PROMPT_COMMAND='history -a' # Save history whenever prompt displayed
HISTFILESIZE=5000

## Read in aliases and functions
source ~/.kshrc

## Modify prompt for Bash - use % instead of $
PS1="${PS1%\$ }% "

## Bash completion for stack (Haskell)
eval "$(stack --bash-completion-script stack)"

## Configure Anaconda3 Python Distribution
if [[ -d ~/opt/anaconda3 ]]
then
    # Have not tested out under ksh - so left this here
    source ~/opt/anaconda3/etc/profile.d/conda.sh
fi
