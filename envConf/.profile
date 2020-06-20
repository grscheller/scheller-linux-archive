#
#   ~/.profile
#

# shellcheck shell=sh
# shellcheck source=/dev/null

export DOT_PROFILE_SOURCED=1

## Get functions and aliases if ksh or bash
#
#  Configuration of an initial interactive
#  environment is handled by the rc files
#  since there is no guarentee that this
#  file will ever be sourced.

MyShell=${0#-}; MyShell=${MyShell##*/}

[ "$MyShell"X = bashX ] &&
    [ -r ~/.bashrc ] &&
    . ~/.bashrc

[ "$MyShell"X = mkshX -o "$MyShell"X = mkshX ] &&
    [ -r ~/.kshrc ]  &&
    export ENV=~/.kshrc

## Perform other tasks unique to actual login shells
