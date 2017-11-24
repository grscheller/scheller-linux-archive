# linuxHome
This project is a stripped down version of the Linux environment syncing
mechanisms I use at work.

## Description:
Mechanism to share a common set of Linux Bash configuration files between
various systems.  Also useful when several systems share the same home
directory via NFS.

## Background:
* At work this project base directory is ~/devel/linuxHome and contains a
  .git repository.
* It is NOT designed to work directly out of a scheller-linux-archive GitHub
  clone.
* Password-less ssh needs to be set up for this project to work.  For info
  on how to do this, see the "Secure Shell" section from
  [UnixCommands.txt](../info/UnixCommands.txt).
* You will need to adjust the system names to match your network.  At work,
  DNS aliases are set up to match the nice names used in the scripts to the
  truely ugly names our system admins are forced to give our systems.
* These scripts utilize shell functions defined in .bashrc.
* I have scaled down the example .bashrc and .bash_profile files contained
  in this project somewhat.

## User scripts:
### [pushHome](bin/pushHome)
* Transfer linuxHome package to your various linux systems.
* Run this on the system you maintain the project's git repo.
* Changes to this script don't get picked up in your ~/bin directory until
  the next time it is run.

### [installHome](bin/installHome)
* Installs linuxHome package locally and then configures your home directory.
* When you initially bootstrap this onto a new system, you will need to
  create a ~/catch directory on the new system.  After pushing the code,
  copy this script from what pushHome deposits into ~/catch directory and
  run it.
* Changes to this script don't get picked up in your ~/bin directory until
  the next time it is run.

## Internal scripts:
### [install_linuxHome](install_linuxHome)
* Updates your home directory from the copy of linuxHome package installed
  locally.  Can be run directly, but usually called from installHome.

