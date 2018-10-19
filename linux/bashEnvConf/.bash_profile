#
# ~/.bash_profile
#
#  Configure initial environment of login shells
#  via a hook in .bashrc.
#
#  Then perform any tasks you wish to apply to 
#  new login shells, like console or ssh logins.
#

export VIRGIN_PATH=${VIRGIN_PATH:=$PATH}
export BASH_PROFILE_SOURCED=${BASH_PROFILE_SOURCED:=0}

(( BASH_PROFILE_SOURCED++ ))

if [[ -f .bashrc ]]
then
    # force resource of .bash_initconf
    unset -f bash_initconf_ran
    # shellcheck source=/dev/null
    source ~/.bashrc
fi

# Clean up PATH
[[ -x ~/bin/pathTrim ]] && PATH=$(~/bin/pathTrim "$PATH")

## Perform tasks unique to login shells

# Log when this file gets sourced
echo "$(date): by $(ps h -q $$)" >> ~/.log_source_bash_profile
