" ~/.config/nvim/init.vim
"
"   The commands in this file are called "vim script"
"   and are run as if in vim command mode.
"

" Setup the Plug plugin manager
"
" Initial bootstrap by manually
" installing it into the right place:
"
"   $ curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
"     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
"
" and then from within nvim run
"
"   :PlugInstall
"
call plug#begin('~/.local/share/nvim/plugged')
" Provide syntax checking for a variety of languages
Plug 'vim-syntastic/syntastic'
" Provide Rust file detection, syntax highlighting,
" formatting, syntastic integration, and more
Plug 'rust-lang/rust.vim'
" Extend */# functionality while in visual mode
Plug 'nelstrom/vim-visual-star-search'
" Surrond text objects with matching (). {}. '', etc
Plug 'tpope/vim-surround'
" Enable repeating supported plugin maps with "." 
Plug 'tpope/vim-repeat'
" Indent text objects; defines 2 new text objects
" based on indentation levels, i and I
Plug 'michaeljsmith/vim-indent-object'
" Visualizes undo history; switch between undo branches
Plug 'mbbill/undotree'
" Shows what is in registers
" extends " and @ in normal mode and <CTRL-R> in insert mode
Plug 'junegunn/vim-peekaboo'
" Extend <ctrl>-A <ctrl>-X to work with dates and not just numbers
Plug 'tpope/vim-speeddating'
call plug#end()

" Configure new user settings for Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

" Force all files ending in .md, besides just README.md,
" to be intepreted as MarkDown and not Modula-2
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Set default encoding - Use utf-8
set encoding=utf-8
set fileencoding=utf-8

" Set the default language to US English for spell checking
set spelllang=en_us

" Set default tabstops and replace tabs with spaces
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" Configure vim to use mouse in normal mode only
set mouse=n
"set mouse=a

set history=5000  " Number lines of command history to keep
set scrolloff=3   " Keep cursor away from edge of window
set backspace=indent,eol,start  " More powerful backspacing

" Make tab completion in command mode more efficient
set wildmenu
set wildmode=longest:full,full

" Open new Vim windows below/right of active window
set splitbelow
set splitright

