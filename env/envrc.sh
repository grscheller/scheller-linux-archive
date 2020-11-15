#!/bin/sh
# shellcheck shell=sh
# shellcheck source=/dev/null
#
#    ~/.envrc
#
#  Configure initial values of $PATH and environment
#  variables you wish child processes, perhaps other
#  shells, to initially inherit.
#
#  This was traditionally done in .profile or .bash_profile
#  whenever a login shell was created.  Unfortunately,
#  in most Desktop Environments, the shells in terminal
#  emulators are not decendant from login shells.
#  We can no longer assume that .profile ever gets sourced.
#

## Sentinel value to mark completion ofan initial environment configuration
export ENV_INIT_LVL=${ENV_INIT_LVL:=0}
ENV_INIT_LVL=$(( ENV_INIT_LVL + 1 ))

if ~/bin/digpath.sh -q nvim 
then
    export EDITOR=nvim
    export VISUAL=nvim
elif ~/bin/digpath.sh -q vim 
then
    export EDITOR=vim
    export VISUAL=vim
else
    export EDITOR=vi
    export VISUAL=vi
fi

# Set locale so commandline tools & other programs default to unicode
export LANG=en_US.utf8

## Python configuration
export PYTHONPATH=lib:../lib
export PIP_REQUIRE_VIRTUALENV=true

## Construct the shell's PATH for all my different computers
#
#  Non-existent path and duplicate path elements
#  will be dealt with near end of script via ~/bin/pathtrim
#  

# Save original PATH
[ -z "$VIRGIN_PATH" ] && export VIRGIN_PATH="$PATH"

## Chocolatey for Windows when in MSYS2 or CYGWIN
PATH=$PATH:/cygdrive/c/ProgramData/chocolatey/bin
PATH=$PATH:/c/ProgramData/chocolatey/bin

# For Termux environment on my Android cell phone
PATH=$PATH:/data/data/com.termux/files/usr/bin/applets

# Put home bin directory near end
PATH=$PATH:~/bin

# Put a relative bin directory at end of PATH, this is for
# projects where the user takes up residence in the project's
# root directory.
PATH=$PATH:bin 

# For Haskell locally contained and administered via stack
PATH=~/.local/bin:$PATH

# Location Rust Toolchain
PATH=~/.cargo/bin:$PATH

# Locally installed SBT (Scala Build Tool)
PATH=~/opt/sbt/bin:$PATH
PATH=/c/Program\ Files\ \(x86\)/sbt/bin:$PATH
# Path to Java on Windows 10
PATH=/c/Program\ Files\ \(x86\)/Common\ Files/Oracle/Java/javapath:"$PATH"

# Clean up PATH - remove duplicate and non-existent path entries
[ -x ~/bin/pathtrim ] && PATH=$(~/bin/pathtrim)

## Information for ssh configuration
#
#                  Host-Name            port  login
export    VOLTRON='rvsllschellerg2        22  schelleg'
export    GAUSS17='192.168.1.22        31502  grs'
export     EULER7='euler7                 22  grs'

## Setup ENV Evironment variable if not already set
if [ -z "$ENV" ]
then
  MyShell=${0#-}; MyShell=${MyShell##*/}
  case "$MyShell"X in
    bashX)
        :
        ;;
    kshX)
        if [ -r ~/.kshrc ]; then
            export ENV=~/.kshrc
        fi
        ;;
    mkshX)
        if [ -r ~/.mkshrc ]; then
            export ENV=~/.mkshrc
        elif [ -r ~/.kshrc ]; then
            export ENV=~/.kshrc
        fi
        ;;
    shX)
        if [ -r ~/.shrc ]; then
            export ENV=~/.shrc
        fi
        ;;
    dashX)
        if [ -r ~/.dashrc ]; then
            export ENV=~/.dashrc
        fi
        ;;
    esac
fi

unset MyShell
