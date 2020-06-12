#
#   ~/.profile
#

# shellcheck shell=sh
# shellcheck source=/dev/null

export DOT_PROFILE_SOURCED=1

## Get functions and aliases if ksh or bash
MyShell=${0#-}; MyShell=${MyShell##*/}
[ "$MyShell"X == bashX ] && [ -r ~/.bashrc ] && . ~/.bashrc
[ "$MyShell"X == kshX ]  && [ -r ~/.kshrc ]  && . ~/.kshrc

## Perform other tasks unique to actual login shells
printf '%s\n' "$HOME/.profile sourced"
