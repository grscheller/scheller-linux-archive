# linuxHome
This project is a stripped down version of the Linux environment<br>
syncing mechanisms I use at work.

## Description:
Mechanism to share a common set of Linux Bash configuration files<br>
between various systems.  Also useful when several systems share<br>
the same home directory via NFS.

## Background:
* At work this project base directory is ~/devel/linuxHome and<br>
  contains a .git repository.
* It is NOT designed to work directly out of a
  scheller-linux-archive GitHub clone.
* Password-less ssh needs to be set up for this project to work.<br>
  See the "Secure Shell" section file 
  [UnixCommands](../info/UnixCommands.txt)<br>
  for info on how to do this.
* You will need to adjust the system names to match your network.<br>
  At work, DNS aliases are set up to match the nice names used in<br>
  the scripts to the truely ugly names our system admins are forced<br>
  to give our systems.
* These scripts utilize shell functions defined in .bashrc.
* I have scaled down the example .bashrc and .bash_profile
  files contained in this project somewhat.

## User scripts:
### [pushHome](bin/pushHome)
* Transfer linuxHome package to your various linux systems.
* Run this on the system you maintain the projects git repo.
* Changes to this script don't get picked up in<br>
  your ~/bin directory until the next time it is run.

### [installHome](bin/installHome)
* Installs linuxHome package locally and then configures your
  home directory.
* When you initially bootstrap this on a new system, you may need to<br>
  extract this script from what pushHome deposits into the ~/catch
  directory.
* Changes to this script don't get picked up in your ~/bin directory<br>
  until the next time it is run.

## Internal scripts:
### [install_linuxHome](install_linuxHome)
* Updates your home directory from the copy of linuxHome package
  installed locally.

