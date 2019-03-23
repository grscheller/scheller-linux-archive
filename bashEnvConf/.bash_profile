## Configure the initial shell environment of login shells
#
#   ~/.bash_profile
#

# shellcheck shell=bash
# shellcheck source=/dev/null

export BASH_PROFILE_LVL=${BASH_PROFILE_LVL:=0}
((BASH_PROFILE_LVL++))

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
# Add user modules if on HPCMP Supercomputers, we always use ssh.
if [[ -n $MODULEPATH ]]
then
    if [[ -d ~/grs_modulefiles ]]
    then
        MODULEPATH="$MODULEPATH:~/grs_modulesfiles"
    fi
    [[ -x ~/bin/pathtrim ]] && MODULEPATH=$(~/bin/pathtrim "$MODULEPATH")
fi

# Turn on Network Manager proxy if ~/.proxy_env exists
[[ -f ~/.proxy_env ]] && proxyUp
