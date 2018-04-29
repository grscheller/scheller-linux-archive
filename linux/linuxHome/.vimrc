" .vimrc
"
" A very simplistic .vimrc file.  Prevents default.vim
" on random linux distributions from giving vim surprising
" behavior, like overriding /etc/vimrc changes!

" Turn off ugly colors on systems I have not had time to
" tweak the environment.
syntax off

" use utf-8
set encoding=utf-8
set fileencoding=utf-8

" Don't use TABS!!!, replace with 4 spaces when <tab> key is pressed.
" Use <ctrl-v><tab> to actually add a real tab.
set tabstop=4
set shiftwidth=4
set expandtab
