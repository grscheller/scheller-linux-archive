# Configure login shells
#
#   ~/.bash_profile
#

# shellcheck shell=bash
# shellcheck source=/dev/null

echo "GRS entering ~/.bash_profile"

export BASH_PROFILE_SOURCED=${BASH_PROFILE_SOURCED:=0}
(( BASH_PROFILE_SOURCED++ ))

## Get functions and aliases
#  Note: Initial shell environment customizations
#        configured via a hook in .bashrc
#
if [[ -f .bashrc ]]
then
    # force reintializing the shell environment
    unset BASH_INIT_SOURCED
    source .bashrc
fi

## Perform other tasks unique to actual login shells

# Add user modules if on HPCMP Supercomputers, we always use ssh.
if [[ -n $MODULEPATH ]] && [[ -d ~/grs_modulefiles ]]
then
    MODULEPATH="$MODULEPATH:~/grs_modulesfiles"
fi

echo "GRS exiting ~/.bash_profile"

