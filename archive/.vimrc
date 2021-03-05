" Vim configuration file
"
" ~/.vim/vimrc
"
" Minimal Vim configuration in case Neovim not available
"

""" Preliminaries

"" Not sure if these are necessary anymore
filetype off
filetype plugin indent on
syntax enable

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

" Duplicate neovim cursor behavior on xterm/urxvt family of terminals
if &term =~# "xterm\\|rxvt"
  let &t_SI = "\<Esc>[6 q"
  let &t_SR = "\<Esc>[4 q"
  let &t_EI = "\<Esc>[2 q"
endif

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
set ruler           " Show line/column info
set laststatus=2    " Always show the status line
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

" Navigate between windows in normal mode using CTRL-hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Lost <C-l> to clear & redraw screen in normal mode
nnoremap <Leader>l :mode<CR>

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
