# .bash_profile
#
# A minimal .bash_profile with hooks to share linux environments.
# between multiple systems.
#
# Before GIT and when diskspace was expensive, developers
# sometimes shared a development environment.  The reference
# SDE below stand for such a shared development environment.
#
# I also build software under my home directory.
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
## Make sure MANPATH is sane.  Set it to what /etc/man.config
## would cause man to default to if MANPATH not set.
## (Done for CentOS6, not necessary for modern linuxes).
export MANPATH=${MANPATH:=/usr/man:/usr/share/man:/usr/local/man:/usr/local/share/man:/usr/X11R6/man}

#
## Setup shared development environment on shared workstations.
PATH=/home/SDE/bin:$PATH

#
## Java8 Setup
PATH=~/opt/jdk8/bin:$PATH
PATH=/opt/jdk8/bin:$PATH
PATH=/home/SDE/opt/jdk8/bin:$PATH
MANPATH=~/opt/jdk8/man:$MANPATH
MANPATH=/opt/jdk8/man:$MANPATH
MANPATH=/home/SDE/opt/jdk8/man:$MANPATH

#
## Usual user stuff (in case System Admins messed up /etc/profile)
PATH=$PATH:/usr/local/bin:/usr/bin:/bin
 
#
## For external packages built locally in home directory
##   Put early in PATH since the version of CentOS am forced to
##   use at work is so out of date. 
PATH=~/local/bin:$PATH
MANPATH=~/local/share/man:$MANPATH

#
## System Stuff
PATH=$PATH:/sbin:/usr/sbin:/usr/local/sbin

#
## Personal bin directory - policy is to defer to all others' names.
## (Use ~/local/bin to overide others' names)
PATH=$PATH:~/bin:.

#
## Clean up PATHS
[[ -x ~/bin/pathTrim ]] && {
    PATH=$(~/bin/pathTrim $PATH)
    LD_LIBRARY_PATH=$(~/bin/pathTrim $LD_LIBRARY_PATH)
    MANPATH=$(~/bin/pathTrim $MANPATH)
}
