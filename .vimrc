" .vimrc
"
" A simplistic .vimrc file.  Its existence also prevents
" vim when run in a terminal from hijacking the mouse from
" the terminal emulator.  I don't want vim moving its cursor
" based on the mouse.  I want the terminal emulator to
" handle copy and paste with the GUI environment.  When I
" right-click, I want the terminal emulator menu.
"
" Turn off ugly colors, use ":syntax on" if you really want them.
syntax off
" use utf-8
set encoding=utf-8
set fileencoding=utf-8
" Don't use TABS!!!, replace with
" 4 spaces when <tab> key is pressed.
"   When editing makefiles or other archaic files
"   where real tabs are "special", 
"   use <ctrl-v><tab> to actually add a real tab.
set tabstop=4
set shiftwidth=4
set expandtab
