# Bash Environment Configuration

* This directory contains example .bash* and other configuration files.
* For these to fully work, the Bash scripts in the
  [bash-utils](../../bash-utils) directory need to
  be put into your ~/bin directory.

## Background:
### CentOS6 Versions:
The CentOS6 versions are stripped down versions of the ones I use at
work.  CentOS6 is a very old distribution.  I need to be able to
collaborate with contractors using more modern versions of Linux.
Due to binary incompatibilities, I need build more up to date
versions of software toolchains and libraries.  This allows me to
run binaries or build from source software that normally would not
run or compile on CentOS6.

You will notice vestiages of hooks needed for collaborative software
development in a shared file environment, from the old days before
GIT repos and when disk drive space was expensive.  Developers would
share the same set of CVS or SCCS source code control files.

### Arch Linux Versions:
For [ARCH Linux](https://www.archlinux.org/), I have no need to
"override" what ARCH gives me in the official repos.

