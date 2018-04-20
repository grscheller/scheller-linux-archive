" .vimrc

" Setup the Vundle plugin manager
"
"   Initially you will need to bootstrap the Vundle infrastructure
"   by manually cloning it into the right place:
"
"   $ git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
"
set nocompatible    " be iMproved, required by Vundle
filetype off        " required by Vundle

" Set the runtime path to include Vundle
set rtp+=~/.vim/bundle/Vundle.vim

" Initialize Vundle
call vundle#begin()
" Let Vundle manage Vundle, required by Vundle
Plugin 'VundleVim/Vundle.vim'

" GitHub plugins for Vundle to manage

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
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" To turn off colors, use ":syntax off"
syntax on

" use utf-8
set encoding=utf-8
set fileencoding=utf-8

" Set the default language to US English for spell checking.
" To turn on spell checking while in vim,  `:set spell'.
" To turn off,                             `:set nospell'.
set spelllang=en_us
