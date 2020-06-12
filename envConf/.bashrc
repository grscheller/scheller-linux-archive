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

## Ststem wide configurations

# Mechanism used on Redhat & Redhat derived systems
[[ -f /etc/bashrc ]] && source /etc/bashrc

# Debian and Arch Linux derived systems typically
# compile bash with option -DSYS_BASHRC which causes
# bash to source /etc/bash.bashrc before ~/.bashrc.

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

## Read in aliases and functions
source ~/.kshrc
PS1="${PS1%\% }$ "  # For Bash, use $ instead of %

## Make BASH more KSH like
shopt -s extglob  # Turn on extended pattern matching.
shopt -s checkwinsize
shopt -s checkhash
shopt -s cmdhist   # Store multiline commands as single entry
shopt -s lithist   # in history with embedded whitespace.
shopt -s histappend  # Append, don't replace history file.
HISTFILESIZE=5000

## Save history whenever prompt displayed
## and update terminal window title.
case $TERM in
  xterm*|rxvt*|urxvt*|kterm*|gnome*)
    PROMPT_COMMAND='history -a; echo -ne "\033]0;${USER}@${HOST}\007"'
    ;;
  screen)
    PROMPT_COMMAND='history -a; echo -ne "\033_${USER}@${HOST}\033\\"'
    ;;
  *)
    PROMPT_COMMAND='history -a'
    ;;
esac

## Bash completion for stack (Haskell)
eval "$(stack --bash-completion-script stack)"

## Configure Anaconda3 Python Distribution
if [[ -d ~/opt/anaconda3 ]]
then
    # On Windows 10, Anaconda is installed into the
    # MSYS2/Anaconda Prompt world.  We must tell
    # Cygwin to ignore LF characters or the conda
    # shell function will fail.
    if [[ $HOST == CYGWIN ]]
    then
        set -o igncr
    fi
    # Have not tested out under ksh - so left this here
    source ~/opt/anaconda3/etc/profile.d/conda.sh
fi
