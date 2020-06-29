## Unix Environment Configurations
###  Assumptions
* System installed upon is, more or less, POSIX compliant.
* Tested only with Korn or Bash as the login shell
### Shell Configuration
Configuration files and associated scripts to bootstrap
my Linux/UNIX/POSIX Bash, Korn and other Shell environments.
* Shell startup scripts
  * bash
  * ksh
  * dash
  * sh (usually simlink to bash or dash)
* ~/bin Shell scripts
* Vim configuration
* Readline Library configuration
* [installHome](installHome) installs everything into $HOME
  * run from envConf directory
  * use as template for your own version

For these configuration files to fully work, the Bash
scripts in the [bin](bin) directory need to be installed into
the ~/bin directory.  The installHome script installs
configuration files and ~/bin executables.

### Facts
For login shells:
* Bash first sources /etc/profile
* Bash then sources `~/.bash_profile`
  * if not found, sources `~/.bash_login`
  * if not found, sources `~/.profile`
* Ksh first sources /etc/profile
* Ksh then sources `~/.profile`
* Ksh then sources $ENV if it exists as a file
* If $ENV unset, some versions of Ksh will source ~/.kshrc

For non-login interactive shells and non-interactive shells:
* Bash sources `$BASH_ENV` if it exits as a file
* Otherwise, Bash sources ~/.bashrc
* Similarly for Ksh, except it uses the POSIX compliant $ENV

That is what these shell do.  What you do with it, is up to you.

### My philosophy on shell startup
Traditionally, a UNIX shell sets up an initial shell environment
when logging into a system via a login shell.  The shell would
source a file like `~/.profile` to establish an initial $PATH and
export shell variables.  Then it would source a file referenced
by the environment variable $ENV, typically  `~/.shrc` to pick up
shell functions and aliases.

Aliases and shell functions are not exported to the environment
but are picked up afresh with each new Bash session via
sourcing `~/.bashrc`.  (Actually, shell functions can be exported
to the environment, but this is not typically what is done).
By Bash session, I am talking about a completely new instance
of Bash, not just a subshell.

This worked well until the age of Display Managers.  A Display
Manager is an X-windows client that logs you directly into your
X-session.  X-Windows is already running under root and a user
owned Session Manager sets up an Desktop Environment.  From this
environment, client programs, including terminal emulators,
can be launched.  The user is "logged" into the X-Session, __not__
a login shell.  As a result, `.bash_profile` does not get run.

Why not just use .bashrc to configure your initial environment?
The problem is is that .bashrc will force __every__ bash
shell to the __exact same__ configuration, not just the first
initial shell in a terminal window.

When I first started using AT&T System V UNIX, I
would login at a real terminal, sometimes connected directly
to computer, othertimes through a network terminal server,
and, after logging in, be in a login shell.

Later I would telnet or dial into the UNIX host via a DOS/Windows
telnet or hyperterminal pc client.  Again, involking a login
shell.

I first started using a UNIX Desktop Environment, CDE on
Solaris 2.6 with an X-terminal.  An X-terminal was a TCP/IP
networked CRT & keyboard that ran an embedded X-server without
a window manager.  It used the BOOTP protocol to figured out what
Solaris UNIX host to contact and display back an X-windows
"console window" client.  After logging in with such a
"console window", guess what, you were in a ksh login shell.
AFTER logging in, you would use the startx command to launch
a remote Window Manager client to manage the remote applications
displayed in the local X-Windows server.  Back then I think the
Window Manager doubled as the Session Manager (I could be wrong).

You see the pattern?  Configure your initial environment
with `.profile` and used `.kshrc` to configure
aliases and functions.  (Back then I only used functions
in shell scripts and never used aliases at all - I may not 
have even known of `.kshrc`)  One file to establish a baseline
environment for the initial shell invocation, and another to
configure shell behaviors which are to stay consistent across
all subsquent shell invocations.

So, to correctly configure an initial shell environment, I
put a hook in `.bashrc` to source a file called `.env_init`
to set up my initial environment for the initial shell
launched by the terminal window.

Notice that no shell variables get "exported" in .bashrc,
it doesn't have to be unless I want programs other than
bash to see them.  In that case it would be better to put
it in `.bash_init`, my surrogate for `.bash_profile`.
That way, I can change it and not have it changed back
as soon as I launch another instance of bash.

The file `.bash_profile` now just sources `.bashrc` to
set up an initial environment and bring in the aliases and
functions to the login shell.  It can also be used for tasks
tasks truely unique to login shells, like when logging in via
ssh or on the system console.

I have a shell function call tm
```
  # Terminal which inherits environment of parent shell
  tm ()
  {
     if [[ $HOST =~ (Cygwin|MinGW|MSYS2) ]]; then
        ( mintty & )
     elif [[ -x /usr/bin/gnome-terminal ]]; then
        ( /usr/bin/gnome-terminal >&- )
     elif [[ -x /usr/bin/xterm ]]; then
        ( /usr/bin/xterm >/dev/null 2>&1 & )
     else
        printf "tm: warning: suitable terminator not found\n" >&2
     fi
  }
```
which launches a new terminal window running a shell not only
in the __same directory__, but with __the same environment__
as the shell from which I launched it.

My shell script, [pathtrim](bin/pathtrim) trims out non-existent
and duplicate paths from my my $PATH.  Why would I put
non-existent and duplicate paths on my $PATH?
* Systems admins put "helpful" additions into system conf files
* exactly the same `~/.bash*` files can be use on different
  computers, even different OS's.
* After installing the missing directories, from the $HOME
  directory I can source `.bash_profile` to pick them up.

I wish my command line and desktop environments to complement
and interact with each other.  I use shell environments to
help manage my programming environments.  I see the desktop
environment as an extension of the command line environment, not
as the "service" running on an "ignorable" underlying OS.
