" Kein Kompatibilitätsmodus
set nocompatible
set background=dark
syntax on " syntax highlighting on

" Visuelle Einstellungen
set number "Zeilennummern anzeigen
set numberwidth=4 " bis 9999
set laststatus=2 " Statusleiste immer anzeigen
set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%03l,%03v]
 95     "              | | | | |  |   |      |  |     |    |
 96     "              | | | | |  |   |      |  |     |    + current 
 97     "              | | | | |  |   |      |  |     |       column
 98     "              | | | | |  |   |      |  |     +-- current line
 99     "              | | | | |  |   |      |  +-- current % into file
100     "              | | | | |  |   |      +-- current syntax in 
101     "              | | | | |  |   |          square brackets
102     "              | | | | |  |   +-- current fileformat
103     "              | | | | |  +-- number of lines
104     "              | | | | +-- preview flag in square brackets
105     "              | | | +-- help flag in square brackets
106     "              | | +-- readonly flag in square brackets
107     "              | +-- rodified flag in square brackets
108     "              +-- full path to file in the buffer
" Tabsize and autoident
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
