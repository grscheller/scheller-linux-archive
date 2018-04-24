#
# ~/.bash_profile
#
#  Configure initial environment of my login shells.
#
#    Essentually, reconfigure what is handed
#    to me by /etc/profile.
#
#  Note: Configure xfce-terminal to launch new terminals
#        with login shells if using a display manager.
#
#        If using startx, initial environment picked up
#        from console login.
#
#  Note: Non-existent path and duplicate path elements
#        will be dealt with near end of script.
#

# Reverse engineering
export BASH_PROFILE_SOURCED=${BASH_PROFILE_SOURCED:=0}
export VIRGIN_BASH_PATH=${VIRGIN_BASH_PATH:=$PATH}

# Configure interactive shells.
[[ -f ~/.bashrc ]] && source ~/.bashrc

umask 0007

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
[[ -x ~/bin/pathTrim ]] && PATH=$(~/bin/pathTrim $PATH)

# Count number of times file sourced
((BASH_PROFILE_SOURCED++))
