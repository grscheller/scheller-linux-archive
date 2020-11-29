#!/bin/bash
# shellcheck shell=bash
# shellcheck source=/dev/null
##
#  ~/.bashrc
#
# Bash configuration across multiple, more or
# or less, POSIX complient systems.
#
#   Written by Geoffrey Scheller
#   See: https://github.com/grscheller/scheller-linux-archive/env
#

## If not interactive, don't do anything.
[[ $- != *i* ]] && return

## System wide configurations

# Debian and Arch Linux derived systems typically
# compile bash with option -DSYS_BASHRC which causes
# bash to source /etc/bash.bashrc before ~/.bashrc.
#
# Mechanism used on Redhat & Redhat derived systems
[[ -f /etc/bashrc ]] && source /etc/bashrc

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
#    descendant from login shells.
#
export ENV_INIT_LVL=${ENV_INIT_LVL:=0}
((ENV_INIT_LVL < 1)) && source ~/.envrc

## Setup up prompt

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
         set -o igncr  # for Anaconda3 Python conda.sh
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

# Setup 3 line primary prompt and prompt command
PS1='\n[\u@${HOST}: \w]\n$ '
PS2='> '
PS3='#? '
PS4='++ '
PROMPT_COMMAND="$PROMPT_COMMAND;printf '%s' \"$TERM_TITLE\""

## Set default behaviors
set -o vi        # vi editing mode
set -o pipefail  # Return right most nonzero error, otherwise 0.
HISTSIZE=5000
HISTCONTROL="ignoredups"

## Command line utility functions

# Jump up multiple directories
ud () {
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

# Similar to the DOS path command
path () {
   if (( $# == 0 ))
   then
       PathWord="$PATH"
   else
       PathWord="$1"
   fi

   # shellcheck disable=SC2086
   ( IFS=':'; printf '%s\n' $PathWord )
}

# Drill down through $PATH to look for files or directories.
# Like the ksh builtin whence, except it does not stop
# after finding the first instance.  Handles spaces in both
# filenames and directories on $PATH.  Also, shell patterns
# are supported.
#
# Usage: digpath [-q] [-h] file1 file2 ...
#        digpath [-q] [-h] shell_pattern
#
# Options: -q: quiet
#          -h: help
#
# Returns: 0 if a file was found on $PATH
#          1 if no file found on $PATH
#          2 if help option or an invalid option given
#
digpath () (

   usage_digpath () {
       printf 'Usage: digpath [-q] file1 file2 ...\n' >&2
       printf '       digpath [-q] shell_pattern\n'   >&2
       printf '       digpath [-h]\n'                 >&2
   }

   local OPTIND opt
   local quiet_flag=
   while getopts :hq opt
   do
     case $opt in
       q) quiet_flag=1
          ;;
       h) usage_digpath
          return 2
          ;;
       ?) printf 'Error: invalid option %s\n' "$OPTARG"  >&2
          usage_digpath
          return 2
          ;;
     esac
   done

   IFS=':'

   local ii=0  # for array index
   local File match
   local FileList=()

   for File in "$@"
   do
      [[ -z "$File" ]] && continue
      for Dir in $PATH
      do
         [[ ! -d "$Dir" ]] && continue
         if [[ -d "$Dir" ]]
         then
             for match in $Dir/$File
             do
                 [[ -f $match ]] && FileList[((ii++))]="$match"
             done
         fi
      done
   done

   [[ -z $quiet_flag ]] && printf '%s\n' "${FileList[@]}"

   if ((${#FileList[@]} > 0))
   then
       return 0
   else
       return 1
   fi
)

# ax - archive extractor
#   usage: ax <file>
ax () {
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
archJDK () {
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

## Launch Desktop GUI Apps from command line

# Open Desktop or Windows file manager
fm () {
   local DiR="$1"
   [[ -n $DiR ]] || DiR="$PWD"
   # if [[ $HOST =~ (Cygwin|MinGW|MSYS2) ]]
   if [[ $HOST == @(Cygwin|MinGW|MSYS2)* ]]
   then
       explorer "$(cygpath -w "$DiR")"
   else
       xdg-open "$DiR"
   fi
}

# Terminal which inherits environment of parent shell
tm () {
   if [[ $HOST == @(Cygwin|MinGW|MSYS2)* ]]; then
      ( mintty & )
   elif [[ -x /usr/bin/urxvt ]]; then
       ( /usr/bin/urxvt >/dev/null 2>&1 & )
   elif [[ -x /usr/bin/gnome-terminal ]]; then
       ( /usr/bin/gnome-terminal >&- )
   elif [[ -x /usr/bin/xterm ]]; then
       ( /usr/bin/xterm >/dev/null 2>&1 & )
   else
       printf "tm: error: terminal emulator not found\n" >&2
   fi
}

# PDF Reader
ev () ( /usr/bin/evince "$@" >/dev/null 2>&1 & )

# LBRY AppImage
lbry () {
   local LBRY_Dir=~/opt/AppImages
   # shellcheck disable=SC2206
   local LBRY_App=(${LBRY_Dir}/LBRY_*.AppImage)

   (( ${#LBRY_App[@]} > 1  )) && {
       printf "  Error: Multiple LBRY apps found in :\n"
       printf "\t%s\n" "${LBRY_App[@]}"
       return 1
   }

   [[ ${LBRY_App[0]} == ${LBRY_Dir}/LBRY_/*.AppImage ]] && {
       printf '  Error: LBRY app not found in %s\n' "$LBRY_Dir"
       return 1
   }

   ( ${LBRY_App[0]} >/dev/null 2>&1 & )
}

# Firefox Browser
ff () {
   if digpath -q "$FIREFOX"
   then
       ( firefox "$@" >&- 2>&- & )
   else
       printf 'firefox not found\n' >&2
   fi
}

# Google Browser
gb () {
   if digpath -q chrome; then
       ( chrome "$@" >&- 2>&- & )
   elif digpath -q chromium; then
       ( chromium "$@" >&- 2>&- & )
   else
       printf 'Neither chrome nor chromium found\n' >&2
   fi
}

# LibreOffice
lo () ( /usr/bin/libreoffice & )

# LibreOffice writer
low () ( /usr/bin/libreoffice --writer "$@" & )

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

# Website scrapping
#   Pull down a subset of a website
alias Wget='/usr/bin/wget -p --convert-links -e robots=off'
#   Pull down more -- Not good for large websites
alias WgetMirror='/usr/bin/wget --mirror -p --convert-links -e robots=off'

# NVIDIA Daemon
#   keeps card active when not running X-Windows
alias nv-pd='sudo /usr/bin/nvidia-persistenced --user grs --persistence-mode'
#   Activate and Deactivate respectfully.
#      Communicates with above daemon if running, otherwise
#      directly with card in a deprecated manner.
alias nv-off='sudo /usr/bin/nvidia-smi -pm 0'
alias nv-on='sudo nvidia-smi -pm 1'

## SSH related functions, variables and aliases

# Restart SSH key-agent and add your private
# key, which is located here: ~/.ssh/id_rsa
alias addkey='eval $(ssh-agent) && ssh-add'

# Make sure git asks for passwords on the command line
unset SSH_ASKPASS

# SSH to another system
sshToSystem () {
   local system=$1
   local port=$2
   local user=$3

   ssh -p "${port}" "${user}@${system}"
}

# Push files to another system
toSystem () {
   local system=$1
   local port=$2
   local user=$3
   shift 3

   scp -P "${port}" -r "$@" "${user}@${system}:catch"
}

# Pull files to another system
fromSystem () {
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

## Bash completion for stack (Haskell)
if digpath -q stack
then
    eval "$(stack --bash-completion-script stack)"
fi

## Configure Anaconda3 Python Distribution (MSYS2 & Cygwin)
if [[ -d ~/opt/anaconda3 ]]
then
    source ~/opt/anaconda3/etc/profile.d/conda.sh
fi

## Make sure other shells have their correct environments
alias ksh='ENV=~/.kshrc ksh'
alias sh='ENV=~/.shrc sh'
alias ash='ENV=~/.shrc ash'
alias dash='ENV=~/.shrc dash'
