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
```
   " .vim/vimrc
   " 
   "   The commands in the file are
   "   run as if in vim command mode.
   "   
   "   First /etc/vimrc is sourced, which should first source
   "   your distribution's vim-package related configuration
   "   files and then your system admins "helpful" system-wide
   "   configuration changes.  Then this file is sourced.
   "
   "   Warning: If neither ~/.vimrc nor ~/.vim/vimrc exist,
   "   vim will source a "defaults.vim" file (unless the SAs
   "   put "let skip_defaults_vim=1" in /etc/vimrc).  This can
   "   very well overide behavior in /etc/vimrc!  Not knowing
   "   about the existence of this mechanism can be very
   "   confusing to new and intermediate vim users.  Simply
   "   creating an empty ~/.vimrc file can radically change
   "   vim behavior and the user has no clue how to recover
   "   previous desirable features.
   
   " Setup the Vundle plugin manager
   "
   " Initially you will need to bootstrap the Vundle infrastructure
   " by manually cloning it into the right place:
   "
   "   $ git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
   "
   " and then from within vim run
   "
   "   :PluginInstall
   "
   set nocompatible    " be iMproved, required by Vundle
   filetype off        " required by Vundle
   
   " Set the runtime path to include Vundle
   set rtp+=~/.vim/bundle/Vundle.vim
   
   " Initialize Vundle
   call vundle#begin()
   " Let Vundle manage Vundle, required by Vundle
   Plugin 'VundleVim/Vundle.vim'
   
   " GitHub plugins for Vundle to manage:
   
   "   Extend */# functionality while in visual mode
   Plugin 'nelstrom/vim-visual-star-search'
   
   "   Provide syntax checking for a variety of languages
   Plugin 'vim-syntastic/syntastic'
   
   "   Provide Rust file detection, syntax highlighting,
   "   formatting, syntastic integration, and more.
   Plugin 'rust-lang/rust.vim'
   
   call vundle#end()
   filetype plugin indent on   " required by Vundle
   " End setup for the Vundle plugin manager
   
   " Configure new user settings for Syntastic
   set statusline+=%#warningmsg#
   set statusline+=%{SyntasticStatuslineFlag()}
   set statusline+=%*
   
   let g:syntastic_always_populate_loc_list = 1
   let g:syntastic_auto_loc_list = 1
   let g:syntastic_check_on_open = 0
   let g:syntastic_check_on_wq = 0
   
   " Turn on and configure syntaxic color highlighting
   syntax on
   " To turn off colors, use ":syntax off"
   "
   " Set the default color scheme
   colorscheme elflord
   "
   " To select a different color scheme while in vim,
   " tab through
   "
   "   :colorscheme blue
   "
   " first to get the command, then to get the color scheme.
   "
   " Example of a color scheme that does not come with Vim,
   "
   "   From https://github.com/tomasr/molokai, put the
   "   colorscheme file here: ~/.vim/color/molokai.vim
   "
   "   Customizations come before :colorscheme command.
   "
   " let g:molokai_original = 1
   " colorscheme molokai
   "
   " Force all files ending in .md, besides just README.md,
   " to be intepreted as MarkDown and not Modula-2.
   autocmd BufNewFile,BufReadPost *.md set filetype=markdown
   
   " Set default encoding
   "   
   " Use utf-8 (I am a utf-8 fanboy)
   set encoding=utf-8
   set fileencoding=utf-8
   
   " Set the default language to US English for spell checking.
   set spelllang=en_us
   "
   " To turn on spellcheck         :set spell
   " To turn it off spellchecking  :set nospell
   
   " Set default tabstops and replace tabs with spaces
   set tabstop=4
   set shiftwidth=4
   set softtabstop=4
   set expandtab
   
   " Configure vim to use mouse in normal mode only
   set mouse=n
   "
   " See `:help mouse-using' to configure the mouse
   "
   " When enabled, use <shift>-clicks to send mouse
   " events to the terminal emulator instead of vim.
   "
   " To enable full mouse support from within vim,
   "   :set mouse=a
   
   set history=500   " Number lines of command history to keep
   set scrolloff=3   " Keep cursor away from edge of window
   set backspace=indent,eol,start  " More powerful backspacing
   
   " Make tab completion in command mode more efficient
   set wildmenu
   set wildmode=longest:full,full
   
   " Open new Vim windows below/right of active window
   set splitbelow
   set splitright
```
