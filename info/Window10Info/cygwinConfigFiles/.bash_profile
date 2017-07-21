#
# ~/.bash_profile
#
#  Configure initial environment of my login shells.
#
#  Locate: /home/user_name   (cygwin name)
#          C:\cygwin64\home\user_name   (Windows name)
#
#  Note: Non-existent path and duplicate path elements
#        will be dealt with near end of script.  You
#        will need the bash scripts from the bash-utils
#        project into /home/user_name/bin
#

# Configure interactive shells.
[[ -f ~/.bashrc ]] && source ~/.bashrc

# Put the version of Java I installed on path before
# the "native" versions
PATH=/cygdrive/c/'Program Files'/Java/jdk1.8.0_131/bin:$PATH

# My private utils bin first; current directory last.
PATH=~/bin:$PATH:.

# For Haskell locally contained and administered via stack
PATH=/home/geoff/.local/bin:$PATH

# Clean up PATH
[[ -x ~/bin/pathTrim ]] && PATH=$(~/bin/pathTrim $PATH)

# Count number of times file sourced
export BASH_PROFILE_SOURCED=${BASH_PROFILE_SOURCED:=0}
((BASH_PROFILE_SOURCED++))
