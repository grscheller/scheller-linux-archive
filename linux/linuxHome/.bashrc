# .bashrc

# A minimal .bashrc file with hooks to share linux environments.
# between multiple systems.

##
## Source global definitions
if [[ -f /etc/bashrc ]]; then
	source /etc/bashrc
fi

#
## Fix what /etc/profile.d/gnome-ssh-askpass.sh may break,
## so that git asks for passwords on command line.
unset SSH_ASKPASS

#
## Force Unicode preference beyong just awareness
export LANG=en_US.utf8

#
## My work email address
export ME='geoffrey.scheller@work.com'

#
## ls family of aliases
alias l1='ls -1'
alias lc='ls --color=auto'
alias la='ls -a'
alias la1='ls -a -1'
alias ll='ls -ltr'
alias lla='ls -ltra'
alias l.='ls -dA .* --color=auto'
alias lS='ls -Ssr'

#
## Print out process tree
alias pst="ps axjf | sed -e '/^ PPID.*$/d' -e's/.*:...//'"

#
## pop up multiple directories
function ud() {
  upDir=../
    if [[ $1 =~ ^[1-9][0-9]*$ ]]
    then
      for ((ii = 1; ii < $1; ii++))
      do
        upDir=../$upDir
      done
    fi
    cd $upDir
}

#
##
## ssh aliases
alias rygar='/usr/bin/ssh userName@rygar'
alias galaga='/usr/bin/ssh userName2@galaga'
alias frogger='/usr/bin/ssh userName@frogger'
alias foobar='/usr/bin/ssh userName3@bar.mySchool.edu'

# scp shell functions (use via aliases)

function toSystem() {
  user=$1; system=$2
  shift 2
  /usr/bin/scp -r "$@" ${user}@${system}:Catch
}

function fromSystem() {
  user=$1; system=$2
  shift 2
  for each in "$@"
  do
      /usr/bin/scp -r ${user}@${system}:"${each}" .
  done
}

#
## scp aliases - if used in scripts, you need to use
##               "shopt -s expand_aliases" in the script.
## Replace userName with your login on each of these systems

alias toRygar='toSystem userName rygar'
alias fromRygar='fromSystem userName rygar'

alias toGalaga='toSystem userName2 galaga'
alias fromGalaga='fromSystem userName2 galaga'

alias toFoobar='toSystem userName3 foobar.mySchool.edu'
alias fromFoobar='fromSystem userName3 foobar.mySchool.edu'

#
## Misc. bash modifcations
set -o pipefail  # return right most nonzero error, otherwise 0
shopt -s extglob  # turn on extended pattern matching
shopt -s checkwinsize
shopt -s checkhash    # Checks if hashed cmd exists, otherwise search path.

#
## Set up command line history and editing
set -o vi         # Cmdline editting like vi editor
shopt -s cmdhist     # Store multiline commands as single entry in history
shopt -s lithist     # and store with embedded whitespace, not ;.
shopt -s histappend    # Append, don't replace history file.
HISTSIZE=2000
HISTFILESIZE=5000
HISTCONTROL="ignoredups:ignorespace"
PROMPT_COMMAND='history -a'   # Whenever Displaying $PS1,
                              # save the last cmd to disk.

##
## Xterm title setup - display nice names, similar to the DNS aliases,
##                     not the ugly ones given by the system admins
export HOST=${HOSTNAME%%.*}
if [[ $TERM == xterm ]] 
then
     case $HOST in
         drxrytl410kzv)
             HOST=rygar
             ;;
         kseyfdxnzjt52)
             HOST=galaga
             ;;
         ghhebxl15166l)
             HOST=frogger
             ;;
     esac
     PROMPT_COMMAND='history -a; echo -ne "\033]0;${USER}@${HOST}\007"'
fi

## Prompt setup - multiline prompt (try it, you'll like it)
PS1='\n[\u@${HOST} \w]\n$ '
PS2='> '
PS3='> '
PS4='+ '

