#!/bin/ksh
# shellcheck shell=ksh
# shellcheck source=/dev/null
#
#  ~/.envrc
#
# Shell functions and aliases
# 
# Contains functions and aliases common to
# both my ksh and bash ahell environments.
#
# Sourced directly by both ~/.kshrc & ~/.bashrc

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

# Swap cattle names for pet names
HOST=$(hostname); HOST=${HOST%%.*}
case $HOST in
  rvsllschellerg2) HOST=voltron ;;
  rvswlschellerg1) HOST=koala ;;
  rvswlwojcikj1)   HOST=littlejohn ;;
  *) if [[ $(uname) == CYGWIN* ]]; then
         HOST=CYGWIN
         export CYGWIN=winsymlinks:nativestrict
         set -o igncr  # for Anaconda3 Python conda.sh
     elif [[ $(uname) == MSYS* ]]; then
         HOST=MSYS2
         export MSYS=winsymlinks:nativestrict
     elif [[ $(uname) == MINGW64* ]]; then
         HOST=MINGW64
         export MSYS=winsymlinks:nativestrict
     elif [[ $(uname) == MINGW32* ]]; then
         HOST=MINGW32
         export MSYS=winsymlinks:nativestrict
     fi
     ;;
esac

## Terminal window title prompt string
case $TERM in
  xterm*|rxvt*|urxvt*|kterm*|gnome*)
    TERM_TITLE=$'\e]0;'"$(id -un)@${HOST}"$'\007'
    ;;
  screen)
    TERM_TITLE=$'\e_'"$(id -un)@${HOST}"$'\e\\'
    ;;
  *)
    TERM_TITLE=''
    ;;
esac

# Setup 3 line primary promptt
PS1="${TERM_TITLE}"$'\n['"$(id -un)@${HOST}"$': $(relative_pwd)]\n> '
PS2='> '
PS3='#? '
PS4='++ '

## Set default behaviors
set -o vi        # vi editing mode
set -o pipefail  # Return right most nonzero error, otherwise 0.
HISTSIZE=5000
HISTCONTROL="ignoredups"
FCEDIT=vim

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

# ax - archive extractor
# usage: ax <file>
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

function which
{
  (alias; typeset -f) |
    /usr/bin/which \
      --read-alias \
      --read-functions \
      --show-dot \
      --show-tilde "$@"
}

# Convert between various bases
#    hex -> dec     (use capital A-F)
#    hex -> octal
#    hex -> binary 
#    dec -> hex
#    dec -> octal 
#    dec -> binary
#    octal -> hex
#    octal -> dec
#    octal -> bin
#    binary -> hex
#    binary -> dec
#    binary -> octal
function h2d
{
  printf 'ibase=16; obase=A; %s\n'  "$*" | /usr/bin/bc
}
function h2o
{
  printf 'ibase=16; obase=8; %s\n'  "$*" | /usr/bin/bc
}
function h2b
{
  printf 'ibase=16; obase=2; %s\n'  "$*" | /usr/bin/bc
}
function d2h
{
  printf 'obase=16; ibase=A; %s\n'  "$*" | /usr/bin/bc
}
function d2o
{
  printf 'ibase=A; obase=8; %s\n'  "$*" | /usr/bin/bc
}
function d2b
{
  printf 'ibase=A; obase=2; %s\n' "$*" | /usr/bin/bc
}
function o2h
{
  printf 'obase=16; ibase=8; %s\n'  "$*" | /usr/bin/bc
}
function o2d
{
  printf 'obase=A; ibase=8; %s\n'  "$*" | /usr/bin/bc
}
function o2b
{
  printf 'ibase=8; obase=2; %s\n'  "$*" | /usr/bin/bc
}
function b2h
{
  printf 'obase=16; ibase=2; %s\n' "$*" | /usr/bin/bc
}
function b2d
{
  printf 'obase=A; ibase=2; %s\n' "$*" | /usr/bin/bc
}
function b2o
{
  printf 'obase=8; ibase=2; %s\n' "$*" | /usr/bin/bc
}

## Aliases

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
alias l.='ls -dA .*'

alias pst="ps axjf | sed -e '/^ PPID.*$/d' -e's/.*:...//'"
alias bc='bc -q'

## Website scrapping

# Pull down a subset of a website
alias Wget='/usr/bin/wget -p --convert-links -e robots=off'
# Pull down more -- Not good for large websites
alias WgetMirror='/usr/bin/wget --mirror -p --convert-links -e robots=off'

## NVIDIA Daemon
#  keeps card active when not running X-Windows
alias nv-pd='sudo /usr/bin/nvidia-persistenced --user grs --persistence-mode'
# Activate and Deactivate respectfully.
#    Communicates with above daemon if running, otherwise
#    directly with card in a deprecated manner.
alias nv-off='sudo /usr/bin/nvidia-smi -pm 0'
alias nv-on='sudo nvidia-smi -pm 1'

## Setup JDK on Arch
function archJDK
{
  local version="$1"

  if [[ ! $version == @([1-9])*([0-9]) ]] 
  then
     echo "Malformed JDK version number: \"$version\""
     return
  fi
  
  if [[ -d /usr/lib/jvm/java-${version}-openjdk ]]
  then
      export JAVA_HOME=/usr/lib/jvm/java-${version}-openjdk
      if [[ $PATH == /usr/lib/jvm/java-[0-9]+-openjdk/bin:* ]]
      then
          PATH=${PATH#[^:]*:}
      fi
      PATH=$JAVA_HOME/bin:$PATH
  else
      echo "No JDK found for Java \"$version\" in the"
      echo "standard location for Arch: /usr/lib/jvm/"
  fi
}

## Launch Desktop GUI Apps from command line

# Open Desktop or Windows file manager
function fm
{
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
function tm
{
  # if [[ $HOST =~ (Cygwin|MinGW|MSYS2) ]]; then
  if [[ $HOST == @(Cygwin|MinGW|MSYS2)* ]]; then
      ( mintty & )
   elif [[ -x /usr/bin/gnome-terminal ]]; then
       ( /usr/bin/gnome-terminal >&- )
   elif [[ -x /usr/bin/xterm ]]; then
       ( /usr/bin/xterm >/dev/null 2>&1 & )
   else
       printf "tm: warning: terminal emulator not found\n" >&2
   fi
}

# PDF Reader
function ev
{
  ( /usr/bin/evince "$@" >/dev/null 2>&1 & )
}

# LBRY AppImage
function lbry
{
  ( ~/opt/AppImages/LBRY_0.45.2.AppImage >/dev/null 2>&1 & )
}

# Firefox Browser
function ff
{
  local FIREFOX=firefox
  [[ -x /usr/bin/firefox ]] && FIREFOX=/usr/bin/firefox
  [[ -x /usr/local/bin/firefox ]] && FIREFOX=/usr/local/bin/firefox
  ( $FIREFOX "$@" >&- 2>&- & )
}

# Google Browser
function gb
{
  local CHROME=chrome
  [[ -x /usr/bin/chrome ]] && CHROME=/usr/bin/chrome
  [[ -x /usr/local/bin/chrome ]] && CHROME=/usr/local/bin/chrome
  [[ -x /opt/google/chrome/chrome ]] && CHROME=/opt/google/chrome/chrome
  ( $CHROME "$@" >&- 2>&- & )
}

## LibreOffice
function lo
{
  ( /usr/bin/libreoffice & )
}

# LibreOffice writer
function low
{
  ( /usr/bin/libreoffice --writer "$@" & )
}

## SSH related functions, variables and aliases

# Restart SSH key-agent and add your private
# key, which is located here: ~/.ssh/id_rsa
alias addkey='eval $(ssh-agent) && ssh-add'

# Make sure git asks for passwords on the command line
unset SSH_ASKPASS

function sshToSystem
{
  local system=$1
  local port=$2
  local user=$3

  ssh -p "${port}" "${user}@${system}"
}

function toSystem
{
  local system=$1
  local port=$2
  local user=$3
  shift 3

  scp -P "${port}" -r "$@" "${user}@${system}:catch"
}

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

#  Single quotes intentional
alias voltron='sshToSystem ${VOLTRON}'
alias toVoltron='toSystem ${VOLTRON}'
alias fromVoltron='fromSystem ${VOLTRON}'

alias koala='sshToSystem ${KOALA}'
alias toKoala='toSystem ${KOALA}'
alias fromKoala='fromSystem ${KOALA}'

alias littlejohn='sshToSystem ${LITTLEJOHN}'
alias toLittlejohn='toSystem ${LITTLEJOHN}'
alias fromLittleJohn='fromSystem ${LITTLEJOHN}'

alias gauss17='sshToSystem ${GAUSS17}'
alias toGauss17='toSystem ${GAUSS17}'
alias fromGauss17='fromSystem ${GAUSS17}'

alias maxwell4='sshToSystem ${MAXWELL4}'
alias toMaxwell4='toSystem ${MAXWELL4}'
alias fromMaxwell4='fromSystem ${MAXWELL4}'

alias euler7='sshToSystem ${EULER7}'
alias toEuler7='toSystem ${EULER7}'
alias fromEuler7='fromSystem ${EULER7}'

