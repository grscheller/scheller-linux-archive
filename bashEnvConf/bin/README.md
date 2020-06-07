## Bash Utilities
Theses are the bash utilities I put into my Linux ~/bin directory.
They are writen in Bash and need Bash to run.  Some may need at
least Bash 4+ to run.

Design goals are:
* In POSIX compliant environment, these should "always work"
* In a "sufficiently" POSIX like environment, should "more or less work"
* In a non-POSIX environment, your "mileage will vary"

### [path](path)
* Spreads $PATH out in a more user readable form
* Output appropriate as input to other shell commands
```
   path | grep home
   realpath $(path)
```
### [pathtrim](pathtrim)
* Used in my Bash startup files.  Useful when $HOME directory,
  or just the startup files, is shared between several systems.
* Trims off duplicate and non-existant director $PATH
```
   Example: PATH=$(~/bin/pathtrim $PATH)
```
### [rt](rt)
* Launch rtorrent Bit-Torrent peer-to-peer ncurses based CLI program.
### [spin](spin)
* Spin a curser around.
* Handy to keep ssh connections alive
```
   Usage: spin

   Hit any key, except <space> or <enter>, to terminate.
```
### [viewJarManifest](viewJarManifest)
* View the manifest list of a jar file.
```
   Usage: viewJarManifest someJarFile.jar
```
### [digpath](digpath)
* Drill down through $PATH to look for files or directories.
* Like ksh builtin whence, except doesn't stop after finding
  first instance.
* Handles spaces in file names and directories.
* Shell patterns supported.
```
   Usage: whence file1 file2 ...
   Example: digpath 'pyth*' 'ghc*' 'filename with spaces'
```
### [monitor](monitor)
* While running, maintain a log of who is on the system
```
