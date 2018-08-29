# .bash_profile
#
# A minimal .bash_profile with hooks to share linux environments.
# between multiple systems.
#
# Before GIT and when diskspace was expensive, developers
# sometimes shared a development environment.  The reference
# SDE below stand for such a shared development environment.
#
# Software built under my home directory installed under ~/local
# Third party software installed under ~/opt
#
# Non-existent path and duplicate path elements will be delt 
# with near end of script.

#
## Get aliases and functions
[[ -f ~/.bashrc ]] &&  . ~/.bashrc

#
## Set default mask so other members of a unix group can share a
## software development environment (SDE).
umask 0007

#
## Setup shared development environment on shared workstations.
PATH=/home/SDE/bin:$PATH

#
## Java8 Setup
PATH=~/opt/jdk8/bin:$PATH
MANPATH=~/opt/jdk8/man:$MANPATH

#
## For external packages built locally in home directory,
## put early in PATH to override what is installed on system.
PATH=~/local/bin:$PATH
MANPATH=~/local/share/man:$MANPATH

#
## Personal bin directory.
PATH=$PATH:~/bin:.

#
## Clean up PATHS
[[ -x ~/bin/pathTrim ]] && {
    PATH=$(~/bin/pathTrim $PATH)
    LD_LIBRARY_PATH=$(~/bin/pathTrim $LD_LIBRARY_PATH)
    MANPATH=$(~/bin/pathTrim $MANPATH)
}
