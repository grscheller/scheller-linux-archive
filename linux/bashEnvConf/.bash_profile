#
# ~/.bash_profile
#
#  Configure initial environment of my login shells,
#  also, initial environment of terminal windows via
#  a hook in .bashrc.
#
#  Essentually, reconfigure what is handed
#  to me by /etc/profile or some damn gui
#  desktop environment.
#
#  Non-existent path and duplicate path elements
#  will be dealt with near end of script.  Can be
#  run multiple times to reinitialize an environment.
#

export VIRGIN_PATH=${VIRGIN_PATH:=$PATH}
export BASH_PROFILE_SOURCED=${BASH_PROFILE_SOURCED:=0}

# Count number of times file sourced
(( BASH_PROFILE_SOURCED++ ))

# Configure what is consistent accross all interactive shells.
if [[ -f .bashrc ]]
then
    # shellcheck source=/dev/null
    source ~/.bashrc
fi

umask u=rwx,g=rwx,o=

export EDITOR=vim
export VISUAL=vim

## Python pip configuration
export PIP_REQUIRE_VIRTUALENV=true

## For Haskell locally contained and administered via stack
PATH=~/.local/bin:$PATH

## Location Rust Toolchain
PATH=~/.cargo/bin:$PATH

## Locally installed SBT (Scala Build Tool)
PATH=~/opt/sbt/bin:$PATH

# Use ~/local/bin for stuff I hope Arch will eventually
# supercede what I install in my home directory.
PATH=$PATH:~/local/bin

# My private utils bin and current directory last.
PATH=$PATH:~/bin:.

# Clean up PATH
[[ -x ~/bin/pathTrim ]] && PATH=$(~/bin/pathTrim "$PATH")
