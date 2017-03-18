# Bash Environment Configuration

* This directory contains example .bash* and other configuration files.
* For these to fully work, the Bash scripts in the
  [bash-utils](../../bash-utils) directory need to be put into your
  ~/bin directory.

## Background:
### CentOS6 Versions:
The CentOS6 versions are stripped down versions of the ones I use at<br>
work.  CentOS6 is a very old distribution.  I need to be able to<br>
collaborate with contractors using more modern versions of Linux.<br>
Due to binary incompatibilities, I need build more up to date<br>
versions of software toolchains and libraries.  This allows me to<br>
run binaries or build from source software that normally would not<br>
run or compile on CentOS6.

You will notice vestiages of hooks needed for collaborative software<br>
development in a shared file environment, from the old days before<br>
GIT repos and when disk drive space was expensive.  Developers would<br>
share the same set of CVS or SCCS source code control files.

### Arch Linux Versions:
For [ARCH Linux](https://www.archlinux.org/), I have no need to "override"<br>
what ARCH gives me in the official repos.

