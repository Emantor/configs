" Kein Kompatibilitätsmodus
set nocompatible
set background=dark
syntax on " syntax highlighting on

" Visuelle Einstellungen
set number "Zeilennummern anzeigen
set numberwidth=4 " bis 9999
set laststatus=2 " Statusleiste immer anzeigen
set statusline=%F%m%r%h%w[%L][%{strlen(&fenc)?&fenc:'none'},%{&ff}]%y[%p%%][%03l,%03v]
"              | | | | |  |   |                               |      |  |     |    |
"              | | | | |  |   |                               |      |  |     |    + current 
"              | | | | |  |   +-- file encoding (UTF-8,...)   |      |  |     |       column
"              | | | | |  |                                   |      |  |     +-- current line
"              | | | | |  |                                   |      |  +-- current % into file
"              | | | | |  |                                   |      +-- current syntax in 
"              | | | | |  |                                   |          square brackets
"              | | | | |  |                                   +-- current fileformat
"              | | | | |  +-- number of lines
"              | | | | +-- preview flag in square brackets
"              | | | +-- help flag in square brackets
"              | | +-- readonly flag in square brackets
"              | +-- rodified flag in square brackets
"              +-- full path to file in the buffer
" Tabsize and autoident
set smartindent
set tabstop=2
set shiftwidth=2
set expandtab
" Remap jj to esc 
inoremap jj <esc>
" foldmethod
set foldmethod=syntax

" Colorscheme
colorscheme molokai
