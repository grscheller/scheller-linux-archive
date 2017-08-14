#
#  ~/.bashrc
#
#  Configure Cygwin to integrate well with Windows
#  but not necessarily with Powershell. 
#
#  Locate: /home/user_name   (cygwin name)
#          C:\cygwin64\home\user_name   (Windows name)
#
#

## If not running interactively, don't configure anything.
if [[ $- != *i* ]]
then
    # Count number of times file sourced noninteractively
    export BASHRC_NON_INTERACTIVE=${BASHRC_NON_INTERACTIVE:=0}
    ((BASHRC_NON_INTERACTIVE++))
    return
fi

## Bash customizations when running interactively

# /etc/profile sets 022, removing write perms to group + others.
# Set a more restrictive umask: i.e. no exec perms for others:
umask 027

set -o notify  # Don't wait until next prompt to report bg jobs status.
set -o pipefail  # Return right most nonzero error, otherwise 0.
shopt -s extglob  # Turn on extended pattern matching.
shopt -s checkwinsize
shopt -s checkhash # Checks if hashed cmd exists, otherwise search path.

# Command line history editing and terminal title
set -o vi         # Cmdline editting like vi editor
shopt -s cmdhist     # Store multiline commands as single entry
shopt -s lithist     # in history with embedded whitespace.
shopt -s histappend    # Append, don't replace history file.
HISTSIZE=5000
HISTFILESIZE=5000
HISTCONTROL="ignoredups"
HOST=${HOSTNAME%%.*}
case $TERM in
  xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
    PROMPT_COMMAND='history -a; echo -ne "\033]0;${USER}@${HOST}\007"'
    ;;
  screen)
    PROMPT_COMMAND='history -a; echo -ne "\033_${USER}@${HOST}\033\\"'
    ;;
  *)
    PROMPT_COMMAND='history -a'
    ;;
esac

# 3 line prompt with pwd
PS1='\n[\u@${HOST}: \w]\n\$ '
PS2='> '
PS3='> '
PS4='+ '

## Aliases and Functions

unalias rm 2> /dev/null
unalias ls 2> /dev/null
alias lc='ls --color=auto'
alias l1='ls -1'
alias la='ls -a'
alias ll='ls -ltr'
alias lla='ls -ltra'
alias l.='ls -dA .* --color=auto'
alias pst="ps axjf | sed -e '/^ PPID.*$/d' -e's/.*:...//'"
alias which='alias | /usr/bin/which --tty-only --read-alias --show-dot --show-tilde'
alias vi='vim'
alias gvim='gvim -c "colorscheme desert"'

## wget aliases to pulldown websites

# Pull down a subset of a website
alias Wget='wget -p --convert-links -e robots=off'

# Pull down more -- Not good for large websites
alias WgetMirror='wget --mirror -p --convert-links -e robots=off'

## Count number of time file sourced interactively
export BASHRC_SOURCED_INTERACTIVELY=${BASHRC_SOURCED_INTERACTIVELY:=0}
((BASHRC_SOURCED_INTERACTIVELY++))
