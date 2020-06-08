## Configure the initial shell environment of login shells
#
#   ~/.bash_profile
#

# shellcheck shell=bash
# shellcheck source=/dev/null

## Get functions and aliases
#
#  Note: Initial shell environment customizations actually
#        configured via ~/,bash_init hook in ~/.bashrc
#  Note: Safe to resource ~/.bash_profile to straighten
#        out a messed up environment
#
if [[ -f ~/.bashrc ]]
then
    # force reintializing the initial shell environment
    export BASH_PROFILE_SOURCED=1
    source ~/.bashrc
fi

## Perform other tasks unique to actual login shells
echo "$HOME/.bash_profile sourced"
