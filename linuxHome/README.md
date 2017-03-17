# linuxHome
This project is a stripped down version of the Linux
environment syncing mechanisms I use at work.

## Description:
Mechanism to share a common set of Linux Bash configuration
files between various systems.  Also useful when several 
systems share the same home directory via NFS.

## Background:
* At work this project base directory is ~/devel/linuxHome
  and contains a .git repository.  It is NOT designed to
  work directly out of a scheller-linux-archive GitHub clone.

* Password-less ssh needs to be set up for this project
  to work.  See the "Secure Shell" section file 
  ../info/UnixCommands.txt for info on how to do this.
  You will need to adjust the system names to match your
  network.  At work, DNS aliases are set up to match nice
  names I use in the scripts to the truely ugly names our
  system admins are forced to give to our systems.

* These scripts utilize shell functions defined in .bashrc.

* I have scaled down the example .bashrc and .bash_profile
  files contained in this project somewhat.

The scripts:

### pushHome
    * Transfer linuxHome package to your various linux systems.
      Run this on the system you maintain the projects git repo.

### installHome
    * Installs linuxHome package locally and then into your
      home directory.

    * When you initially bootstrap this on a new system,
      you may need to extract this script from what
      pushHome deposits into the ~/catch directory.

    * Changes to this script don't get picked up in
      your ~/bin directory until the next time it is run.

