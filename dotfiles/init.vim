" Neovim configuration file
"
" ~/.config/nvim/init.vim
"

""" Preliminaries

"" Remove Vim misfeatures and vulnerabilities
set nomodeline

"" Improve Vi, not Clone Vi

" More powerful backspacing in insert mode
set backspace=indent,eol,start

" Make tab completion in command mode more useful
set wildmenu
set wildmode=longest:full,full

" Allow gf and :find to use recursive sub-folders
" and find files in the working directory
set path+=.,**
set hidden

"" Set default encoding and localizations
"  Warning: Linux/Unix/UTF-8 oriented
set encoding=utf-8
set fileencoding=utf-8
set spelllang=en_us
set fileformats=unix,mac,dos

""" Personnal preferences

"" Configure features and behaviors

" Setup color scheme
colorscheme ron

" Set default tabstops and replace tabs with spaces
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" Other configurations
set history=10000   " Number lines of command history to keep
set mouse=a         " Enable mouse for all modes
set scrolloff=2     " Keep cursor away from top/bottom of window
set nowrap          " Don't wrap lines
set sidescroll=1    " Horizontally scroll nicely
set sidescrolloff=5 " Keep cursor away from side of window
set splitbelow      " Horizontally split below
set splitright      " Vertically split to right
set hlsearch        " Highlight / search results after <CR>
set incsearch       " Highlight / search matches as you type
set ignorecase      " Case insensitive search,
set smartcase       " ... unless query has caps
set showcmd         " Show partial normal mode commands in lower right corner
set nrformats=bin,hex,octal " bases used for <C-a> & <C-x>,
set nrformats+=alpha        " ... also single letters too

"" Setup key mappings

" Define <Leader> explicitly as a space
nnoremap <Space> <Nop>
let g:mapleader = "\<Space>"

" Clear search highlighting
nnoremap <Leader><Space> :nohlsearch<CR>

" Get rid of all trailing whitespace for entire buffer
nnoremap <Leader>w :%s/\s\+$//<CR>

" Toggle spell checking
nnoremap <Leader>sp :set invspell<CR>

" Reduce keystrokes from :dig to entering digraph
nnoremap <expr> <Leader>k ":dig<CR>a\<C-k>"

" Navigating in insert mode using ALT-hjkl
inoremap <M-h> <Left>
inoremap <M-j> <Down>
inoremap <M-k> <Up>
inoremap <M-l> <Right>

" Navigate between windows in normal mode using CTRL-hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Lost <C-l> to clear & redraw screen in normal mode
nnoremap <Leader>l :mode<CR>

" Resize windows in normal mode using ALT-hjkl
nnoremap <M-h> 2<C-w><
nnoremap <M-j> 2<C-w>-
nnoremap <M-k> 2<C-w>+
nnoremap <M-l> 2<C-w>>

" Toggle between 3 line numbering states via <Leader>n
set nonumber
set norelativenumber

function! MyLineNumberToggle()
    if(&relativenumber == 1)
        set nonumber
        set norelativenumber
    elseif(&number == 1)
        set nonumber
        set relativenumber
    else
        set number
        set norelativenumber
    endif
endfunction

nnoremap <Leader>n :call MyLineNumberToggle()<CR>

""" Setup plugins

"" Add optional plugins bundled with nvim
packadd! matchit   " Add additional matching functionality to %

"" Setup the Plug plugin manager
"
" Bootstrap manually by installing plug.vim into the right place:
"
"   $ curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
"     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
"
" and then from command mode run
"
"   :PlugInstall
"
" Plug Commands:
"   :PlugInstall [name ...] [#threads] Install plugins
"   :PlugUpdate [name ...] [#threads]  Install or update plugins
"   :PlugClean[!]           Remove unlisted plugins
"   :PlugUpgrade            Upgrade Plug itself
"   :PlugStatus             Check the status of plugins
"   :PlugDiff               Diff previous update and pending changes
"   :PlugSnapshot[!] [path] Generate script to restore current plugin state
"
call plug#begin('~/.local/share/nvim/plugged')

" Using vim-airline to configure the statusline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Extend * and # functionality while in visual mode
Plug 'nelstrom/vim-visual-star-search'

" Surrond text objects with matching (). {}. '', etc
Plug 'tpope/vim-surround'

" Extend <C-a> and <C-x> to work
" with dates and not just numbers.
Plug 'tpope/vim-speeddating'

" Enable repeating supported plugin maps with "."
Plug 'tpope/vim-repeat'

" Indent text objects; defines 2 new text objects
" based on indentation levels, i and I
Plug 'michaeljsmith/vim-indent-object'

" Shows what is in registers
" extends " and @ in normal mode and <C-r> in insert mode
Plug 'junegunn/vim-peekaboo'

" Provide syntax checking with Syntastic
Plug 'vim-syntastic/syntastic'

" Provide Rust file detection, syntax highlighting,
" formatting, Syntastic integration, and more
Plug 'rust-lang/rust.vim'

" Provide VimL lint checking via vimlint (below) and vint (pacman)
Plug 'ynkdir/vim-vimlparser'
Plug 'syngan/vim-vimlint'

call plug#end()

"" Configure user settings for Syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_vim_checkers = ['vint', 'vimlint']

" Toggle Synastic into and out of passive mode
nnoremap <Leader>st :SyntasticToggleMode<CR>
