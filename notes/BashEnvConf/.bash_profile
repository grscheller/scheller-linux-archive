#
# ~/.bash_profile
#
#  Configure initial environment of login shells
#  via a hook in .bashrc.
#
#  Non-existent path and duplicate path elements
#  will be dealt with via the pathTrim script.
#
#  Perform any tasks you wish to apply to 
#  new login shells.
#

export VIRGIN_PATH=${VIRGIN_PATH:=$PATH}
export BASH_PROFILE_SOURCED=${BASH_PROFILE_SOURCED:=0}

(( BASH_PROFILE_SOURCED++ ))

if [[ -f .bashrc ]]
then
    # force resource of .bash_initenv
    unset -f bash_init_sourced
    # shellcheck source=/dev/null
    source ~/.bashrc
fi

# Clean up PATH
[[ -x ~/bin/pathTrim ]] && PATH=$(~/bin/pathTrim "$PATH")

## Perform tasks unique to login shells

# Log when this file gets sourced
echo "$(date): by $(ps h -q $$)" >> ~/.log_source_bash_profile
