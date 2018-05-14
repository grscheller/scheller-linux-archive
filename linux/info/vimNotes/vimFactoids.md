## Useful and/or essential Vim factoids

### Vim startup files
Without this information, it can be very frustrating
reverse engineer Vim behavior.  A number of files are
sourced before your editing session starts, where
"sourced" means the commands in these files are entered 
as if in _Command Mode_, but without the initial `:`.

First sourced is /etc/vimrc.  Historically on Unix, this
was the location for system-wide vim configuration changes.
Now a days, this file has a command that sources your Linux
distribution's vim-package related configuration changes.
For Arch Linux this file
is `/usr/share/vim/vimfiles/archlinux.vim`, but the command
only uses the file's base name, its location compiled into
the vim executable.

Your linux system admins may also have added other "helpful"
system-wide configuration changes into either of these files.
possibly even creating other files in the compiled in
location.

Vim next looks for user configuration changes in `~/.vimrc`,
if it does not exist, it then looks in `~/.vim/vimrc`. 

__Warning:__ If neither ~/.vimrc nor ~/.vim/vimrc exist,
vim will source the `defaults.vim` file.  This can
very well overide behavior in `/etc/vimrc` itself!  Not
knowing about the existence of this mechanism can be very
confusing to new and intermediate vim users.  Simply
creating an empty ~/.vimrc file can radically change
vim behavior and the user has no clue how to recover
previous desirable features.  Putting
`let skip_defaults_vim=1` in `/etc/vimrc` will stop
this "feature."

In Arch Linux, the location of this default file is
`/usr/share/vim/vim80/defaults.vim`.

### Buffers in Vim
There are several things in Vim refered to as buffers.  These
are areas that can store text.  The three most important ones
for now are:
* default buffer
* named buffers
* file buffers

##### The default buffer
The default buffer is just the area which `y` and `c` commands
write to and `w` and `c` commands read from by "default."

##### Named buffers
Illustrated in [Basic text editing](basicTextEditing.md),
Named buffers are areas where you can store snippets of text.
They are named `"a` thru `"z`and are essentially
26 independent "clip boards" that are shared between all the file
buffers.

##### File buffers
These are the in memory text associated with a file.  To list
them, use the `:buffers` command.  Amoung other things, this
gives a unique buffer number and filename (if any) associated
with that buffer.

* `:n`           edit next buffer
* `:next`        same as above
* `:prev`        edit previous buffer
*  :edit <file>` edit buffer associated with <file> in current window
*  :e <file>`    same as above, for both, creates new buffer if necessary
* `:buffers`     list buffers
* `:ls`          same as above
* `:b2`          edit buffer 2 in current window (not commonly used)
* `:new`         open new window with a new empty buffer
* `:split`       open new window but use the same buffer
*  :spl          same as above, basically 2 views of same buffer
* `:w`           write buffer to file associated with buffer
*  :w <file>     write buffer to <file>, buffer file association does not change
* `:q`           quit window, if last view, won't let you until changes written to disk
* `:q!`          quit window, abandon any changes if last view

### Using the mouse
When configured to use the mouse, vim will steal the mouse
events from the terminal emulator.  To enable full vim mouse
support, `:set mouse=a` and to disable the mouse and let the
terminal emulator handle all mouse events, `:set mouse=`.

Available mouse options are:

| Option | Mode                                 |
|:------:| ------------------------------------ |
| n      | _Normal Mode_                        |
| v      | _Visual Mode_                        |
| i      | _Insert Mode_                        |
| c      | _Command Mode_                       |
| a      | All previous modes                   |
| h      | All previous modes only when in help |

As a workaround, you can send mouse events directly to the
terminal emulator instead of Vim by holding down the SHIFT
key.

I find that configuring the mouse for anything but _Normal Mode_
pretty useless and counter intuitive.  In _Insert Mode_ I
don't like the middle mouse button repositioning the paste.
In _Normal Mode_ I don't like a click-drag throwing me into
character _Visual Mode_.

### Dealing with whitespace characters:
Tell vim to indicate where line endings and tabs are,

* `:set list`

to return to displaying tabs and line endings normally,

* `:set nolist`

This is a wonderful feature to get rid of tabs and trailing whitespace.

### Spell checking:
Turn spell checking on,

* `:set spell`

turn spell checking off,

* `:set nospell`
