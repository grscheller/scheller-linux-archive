# Bash Utilities:

Theses are the bash utilities I put into my Linux ~/bin directory.

### path -
    * Spreads $PATH out in a more user readable form.
      Also, output more appropriate for use as input
      to other commands.  Examples:
      ```
         path | grep home
         realPath $(path)
      ```
### pathTrim -
    * Used in my .bash_profile.  Useful when $HOME and/or
      bash_profile are/is shared between several systems.
      Trims off duplicate entries and non-existant
      directoris from colen separated Unix paths.
      ```
         Usage: pathTrim colon:separated:list
      ```
### realPath -
    * Resolve symlinks and print out the real path for each
      path given on the commandline.  Example:
      ```
         Usage: realPath /path/to/first/item another/path/to/second/item
      ```
      Works well with whence:
      ```
         realPath $(whence java javac scala python cc gcc ghc)
      ```
### rt -
    * Launch rtorrent Bit-Torrent peer-to-peer ncurses based CLI program.

### spin -
    * Spin a curser around - Handy to keep ssh connections alive
      when they terminate after only 10 minutes of inactivity.
      ```
         Usage: spin
      ```
      Hit any key, except <space> or <enter>, to terminate.

### viewJarManifest -
    * View the manifest list of a *.jar file.
      ```
         Usage: viewJarManifest someJarFile.jar
      ```
### whence -
    * Drill down through $PATH to look for files or directories.
      Like the ksh builtin whence, except it does not stop after
      finding the first instance.  Handles spaces in file names
      and directories on $PATH.  Shell patterns supported.
      ```
         Usage: Whence file1 file2 ...
         Whence 'pattern'
      ```
