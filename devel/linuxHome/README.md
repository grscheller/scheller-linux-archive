This project is a stripped down version of the Linux
environment syncing mechanisms I use at work.

At work this project base directory is ~/devel/linuxHome and
contains a .git repository.  It is NOT designed to work
directly in a scheller-linux-environment GitHub clone.

Also, password-less ssh needs to be set up for this project
to work.  See the "Secure Shell" section file UnixCommands.txt
for info on how to do this.  You will need to adjust the
system names to match your network.  At work, DNS aliases are
set up to match nice names I use in the scripts to the 
truely ugly names our system admins are forced to give to our
systems.  Scripts also utilize shell functions defined in .bashrc.

I have scaled down the example .bashrc and .bash_profile files
contained in this project somewhat.

The scripts:

  pushHome    -> transfer linuxHome package to your various linux systems
    Note: run on the system you maintain the projects git repo

  installHome -> install this package locally and then into your home directory
    Note: When you initially bootstrap this on a new system,
          you may need to extract this script from what
          pushHome deposits into the ~/catch directory.
    Note: Changes to this script don't get picked up in
          your ~/bin directory until the next time it is run.
