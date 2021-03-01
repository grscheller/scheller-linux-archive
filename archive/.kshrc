#!/bin/ksh
# shellcheck shell=ksh
# shellcheck source=/dev/null
##
#  ~/.kshrc
#
# Korn Shell configuration across multiple, more or
# or less, POSIX complient systems.
#
#   Written by Geoffrey Scheller
#   See: https://github.com/grscheller/scheller-linux-archive/env
#

## If not interactive, don't do anything.
[[ $- != *i* ]] && return

## Make sure an initial shell environment is well defined
#
#    Shells in terminal windows not necessarily
#    descendant from login shells.
#
export ENV_INIT_LVL=${ENV_INIT_LVL:=0}
((ENV_INIT_LVL < 1)) && source ~/.envrc

## Setup up prompt

function relative_pwd
{
   if [[ ${PWD:0:${#HOME}} == "$HOME" ]]
   then
       printf '%s' "~${PWD:${#HOME}}"
   else
       printf '%s' "$PWD"
   fi
}

# Adjust Hostname
#  Swap cattle names with pet names
#
#  On Windows:
#   - indicate if Cygwin, MSYS2, MINGW64 or MINGW32 environment
#   - use native NTFS symlinks (need to turn on developer mode)
#
HOST=$(hostname); HOST=${HOST%%.*}
case $HOST in
  rvsllschellerg2) HOST=voltron ;;
  *) if [[ $(uname) == MSYS* ]]; then
         HOST=MSYS2
         export MSYS=winsymlinks:nativestrict
     elif [[ $(uname) == MINGW64* ]]; then
         HOST=MINGW64
         export MSYS=winsymlinks:nativestrict
     elif [[ $(uname) == MINGW32* ]]; then
         HOST=MINGW32
         export MSYS=winsymlinks:nativestrict
     elif [[ $(uname) == CYGWIN* ]]; then
         HOST=CYGWIN
         export CYGWIN=winsymlinks:nativestrict
     fi
     ;;
esac

# Terminal window title prompt string
case $TERM in
  xterm*|rxvt*|urxvt*|kterm*|gnome*)
    TERM_TITLE=$'\e]0;'"$(id -un)@\${HOST}"$'\007'
    ;;
  screen)
    TERM_TITLE=$'\e_'"$(id -un)@\${HOST}"$'\e\\'
    ;;
  *)
    TERM_TITLE=''
    ;;
esac

# Setup 3 line primary prompt
PS1="${TERM_TITLE}"$'\n['"$(id -un)@${HOST}"$': $(relative_pwd)]\n% '
PS2='> '
PS3='#? '
PS4='++ '

## Set default behaviors
set -o vi        # vi editing mode
set -o pipefail  # Return right most nonzero error, otherwise 0.
HISTSIZE=5000
HISTCONTROL="ignoredups"

## Command line utility functions

# Jump up multiple directories
function ud
{
   local upDir=..
   local nDirs="$1"
   if [[ $nDirs == @([1-9])*([0-9]) ]]
   then
       until (( nDirs-- <= 1 ))
       do
           upDir=../$upDir
       done
   fi
   cd $upDir || return
}

# Print out $PATH
function path
{
   if (( $# == 0 ))
   then
       PathWord="$PATH"
   else
       PathWord="$1"
   fi

   # shellcheck disable=SC2086
   ( IFS=':'; printf '%s\n' $PathWord )
}

# ax - archive extractor
#   usage: ax <file>
function ax
{
   if [[ -f $1 ]]
   then
       case $1 in
         *.tar)     tar -xvf "$1"                        ;;
         *.tar.bz2) tar -xjvf "$1"                       ;;
         *.tbz2)    tar -xjvf "$1"                       ;;
         *.tar.gz)  tar -xzvf "$1"                       ;;
         *.tgz)     tar -xzvf "$1"                       ;;
         *.tar.Z)   tar -xZvf "$1"                       ;;
         *.gz)      gunzip "$1"                          ;;
         *.bz2)     bunzip2 "$1"                         ;;
         *.zip)     unzip "$1"                           ;;
         *.Z)       uncompress "$1"                      ;;
         *.rar)     unrar x "$1"                         ;;
         *.tar.xz)  xz -dc "$1" | tar -xvf -             ;;
         *.tar.7z)  7za x -so "$1" | tar -xvf -          ;;
         *.7z)      7z x "$1"                            ;;
         *.cpio)    cpio -idv < "$1"                     ;;
         *) printf 'ax: error: "%s" unknown file type' "$1"  >&2 ;;
       esac
   else
       if [[ -n $1 ]]
       then
           printf 'ax: error: "%s" is not a file' "$1" >&2
       else
           printf 'ax: error: No file argument given' >&2
       fi
   fi
}

# Convert between various bases (use capital A-F for hex-digits)
#
#   Examples: h2d "AA + BB" -> 357
#             h2h "AA + BB" -> 165
#             d2h 357       -> 165
#
h2h () { printf 'ibase=16\nobase=10\n%s\n'   "$*" | /usr/bin/bc; }
h2d () { printf 'ibase=16\nobase=A\n%s\n'    "$*" | /usr/bin/bc; }
h2o () { printf 'ibase=16\nobase=8\n%s\n'    "$*" | /usr/bin/bc; }
h2b () { printf 'ibase=16\nobase=2\n%s\n'    "$*" | /usr/bin/bc; }
d2h () { printf 'ibase=10\nobase=16\n%s\n'   "$*" | /usr/bin/bc; }
d2d () { printf 'ibase=10\nobase=10\n%s\n'   "$*" | /usr/bin/bc; }
d2o () { printf 'ibase=10\nobase=8\n%s\n'    "$*" | /usr/bin/bc; }
d2b () { printf 'ibase=10\nobase=2\n%s\n'    "$*" | /usr/bin/bc; }
o2h () { printf 'ibase=8\nobase=20\n%s\n'    "$*" | /usr/bin/bc; }
o2d () { printf 'ibase=8\nobase=12\n%s\n'    "$*" | /usr/bin/bc; }
o2o () { printf 'ibase=8\nobase=10\n%s\n'    "$*" | /usr/bin/bc; }
o2b () { printf 'ibase=8\nobase=2\n%s\n'     "$*" | /usr/bin/bc; }
b2h () { printf 'ibase=2\nobase=10000\n%s\n' "$*" | /usr/bin/bc; }
b2d () { printf 'ibase=2\nobase=1010\n%s\n'  "$*" | /usr/bin/bc; }
b2o () { printf 'ibase=2\nobase=1000\n%s\n'  "$*" | /usr/bin/bc; }
b2b () { printf 'ibase=2\nobase=10\n%s\n'    "$*" | /usr/bin/bc; }

# Setup JDK on Arch
function archJDK
{
   local version="$1"
   local jdir
   local jver

   if [[ -z $version ]]
   then
       printf "Available Java Versions: "
       for jdir in /usr/lib/jvm/java-*-openjdk
       do
           jver=${jdir%-*}
           jver=${jver#*-}
           printf '%s ' "$jver"
       done
       printf '\n'
       return
   fi

   if [[ ! $version == @([1-9])*([0-9]) ]]
   then
       printf "Malformed JDK version number: \"%s\"\n" "$version"
       return
   fi

   if [[ -d /usr/lib/jvm/java-${version}-openjdk ]]
   then
       export JAVA_HOME=/usr/lib/jvm/java-${version}-openjdk
       if [[ $PATH == /usr/lib/jvm/java-@([1-9])*([0-9])-openjdk/bin:* ]]
       then
           PATH=${PATH#[^:]*:}
       fi
       PATH=$JAVA_HOME/bin:$PATH
   else
       printf "No JDK found for Java \"%s\" in the\n" "$version"
       printf "standard location for Arch: /usr/lib/jvm/\n"
   fi
}

## Aliases

# Remove any inherited misconfigurations
unalias rm 2>&-
unalias ls 2>&-
unalias grep 2>&-
unalias egrep 2>&-
unalias fgrep 2>&-

# ls alias family
alias lc='ls --color=auto'
alias l1='ls -1'
alias la='ls -a'
alias ll='ls -ltr'
alias lh='ls -ltrh'
alias lla='ls -ltra'
alias lha='ls -ltrha'
alias l.='ls -dA .*'

alias pst="ps axjf | sed -e '/^ PPID.*$/d' -e's/.*:...//'"
alias bc='bc -q'
alias nv=nvim

alias digpath='$HOME/.local/bin/digpath.bash'
# alias digpath='$HOME/.local/bin/digpath.sh'

## SSH related functions, variables and aliases

# Restart SSH key-agent and add your private
# key, which is located here: ~/.ssh/id_rsa
alias addkey='eval $(ssh-agent) && ssh-add'

# Make sure git asks for passwords on the command line
unset SSH_ASKPASS

# SSH to another system
function sshToSystem
{
   local system=$1
   local port=$2
   local user=$3

   ssh -p "${port}" "${user}@${system}"
}

# Push files to another system
function toSystem
{
   local system=$1
   local port=$2
   local user=$3
   shift 3

   scp -P "${port}" -r "$@" "${user}@${system}:catch"
}

# Pull files to another system
function fromSystem
{
   local system=$1
   local port=$2
   local user=$3
   shift 3

   local each
   for each in "$@"
   do
       scp -P "${port}" -r "${user}@${system}:${each}" .
   done
}

# Aliases for above functions
alias voltron='sshToSystem ${VOLTRON}'
alias toVoltron='toSystem ${VOLTRON}'
alias fromVoltron='fromSystem ${VOLTRON}'

alias gauss17='sshToSystem ${GAUSS17}'
alias toGauss17='toSystem ${GAUSS17}'
alias fromGauss17='fromSystem ${GAUSS17}'

alias euler7='sshToSystem ${EULER7}'
alias toEuler7='toSystem ${EULER7}'
alias fromEuler7='fromSystem ${EULER7}'

## Make sure other shells have their correct environments
alias bash='ENV= bash'
alias sh='ENV=~/.shrc sh'
alias ash='ENV=~/.shrc ash'
alias dash='ENV=~/.shrc dash'
