# Configure login shells
#
#   ~/.bash_profile
#

# shellcheck shell=bash
# shellcheck source=/dev/null

export BASH_PROFILE_SOURCED=${BASH_PROFILE_SOURCED:=0}
(( BASH_PROFILE_SOURCED++ ))

## Get functions and aliases
#  Note: Initial environment configured
#        via a hook in .bashrc
#
if [[ -f .bashrc ]]
then
    # force resource of .bash_init within .bashrc
    unset BASH_INIT_SOURCED
    source .bashrc
fi

## Perform other tasks unique to actual login shells

# Add user modules if on HPCMP Supercomputers, we always use ssh.
if [[ -n $MODULEPATH ]] && [[ -d ~/grs_modulefiles ]]
then
    MODULEPATH="$MODULEPATH:~/grs_modulesfiles"
fi

## Temporary test - to be removed
thing1 () { printf 'I am thing 1\n'; }
thing2 () { printf 'I am thing 2\n'; }
export thing1
