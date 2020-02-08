# Vim Configuration Files

## Vim startup files
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

__Warning:__ If neither `~/.vimrc` nor `~/.vim/vimrc` exist,
vim will source the `defaults.vim` file.  This can
very well overide behavior in `/etc/vimrc` itself!  Not
knowing about the existence of this mechanism can be very
confusing to new and intermediate vim users.  Simply
creating an empty ~/.vimrc file can radically change
vim behavior and the user has no clue how to recover
previous desirable features.  Putting the line
```
    let skip_defaults_vim=1
```
in `/etc/vimrc` will stop this "feature."

In Arch Linux, the location of this default file is
`/usr/share/vim/vim80/defaults.vim`.

## Sample vimrc files:
The commands in these .vimrc example files are run
as if in vim command mode.  Comments begin with `"`.

### Simplistic .vimrc
```
   " ~/.vimrc
   "
   " A very simplistic .vimrc file.  Prevents default.vim
   " on random linux distributions from giving vim surprising
   " behavior, like overriding /etc/vimrc changes!
   
   " Turn off ugly colors on systems for which 
   " I have not had time to tweak the environment.
   syntax off
   
   " use utf-8
   set encoding=utf-8
   set fileencoding=utf-8
   
   " Don't use TABS!!!, replace with 4 spaces when <tab> key is pressed.
   " Use <ctrl-v> <tab> to actually add a real tab.
   set tabstop=4
   set shiftwidth=4
   set expandtab
```
### Intermediate vim user's vimrc file 
For an example see the
[vimrc](../../bashEnvConf/.vim/vimrc)
that I use.
