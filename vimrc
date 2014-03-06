let $PYTHONPATH="/usr/lib/python3.3/site-packages"
" Kein Kompatibilitätsmodus
set nocompatible
set background=dark
syntax on " syntax highlighting on

" Visuelle Einstellungen
set number "Zeilennummern anzeigen
set numberwidth=4 " bis 9999
set laststatus=2 " Statusleiste immer anzeigen
set statusline=%F%m%r%h%w[%L]%{fugitive#statusline()}[%{strlen(&fenc)?&fenc:'none'},%{&ff}]%y[%p%%][%03l,%03v]
"              | | | | |  |   |                       |                               |      |  |     |    |
"              | | | | |  |   |                       |                               |      |  |     |    + current 
"              | | | | |  |   |                       +-- file encoding (UTF-8,...)   |      |  |     |       column
"              | | | | |  |   |                                                       |      |  |     +-- current line
"              | | | | |  |   +-- Git Status                                          |      |  +-- current % into file
"              | | | | |  |                                                           |      +-- current syntax in 
"              | | | | |  |                                                           |          square brackets
"              | | | | |  |                                                           +-- current fileformat
"              | | | | |  +-- number of lines
"              | | | | +-- preview flag in square brackets
"              | | | +-- help flag in square brackets
"              | | +-- readonly flag in square brackets
"              | +-- modified flag in square brackets
"              +-- full path to file in the buffer
"
" Tabsize and autoident
set smartindent
set tabstop=2
set shiftwidth=2
set expandtab

" Remap jj to esc 
inoremap jj <esc>

" set leader key to comma
let mapleader = ","

" rename current file, via Gary Bernhardt
function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'))
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction
map <leader>n :call RenameFile()<cr>

" rename current file, via Gary Bernhardt
function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'))
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction
map <leader>r :call RenameFile()<cr>

"""Leader Maps
" Git Shortcuts 
map <leader>c :Gcommit<cr>
map <leader>a :Git add %<cr>
map <leader>b :Gblame<cr>
map <leader>l :!clear && git log -p %<cr>
map <leader>d :!clear && git diff %<cr>
" Silver Searcher
map <leader>s :Ag!<space>
" Search word under cursor with Silver Searcher
map <leader>S :Ag! "<C-r>=expand('<cword>')<CR>"
" Write with sudo
noremap <Leader>W :w !sudo tee % > /dev/null

" toggle shortcuts for paste, hlsearch, invlist
nnoremap <F2> :set invpaste paste?<CR>
" set pastetoggle=<F5>
nnoremap <F3> :set invhlsearch hlsearch?<CR>
inoremap <F3> <Esc>:set invhlsearch hlsearch?<CR>a
nnoremap <F4> :set invlist list?<CR>
inoremap <F4> <Esc>:set invlist list?<CR>a
map <F5> :setlocal spell! spelllang=de_de<cr>
imap <F5> <ESC>:setlocal spell! spelllang=de_de<cr>
nnoremap <C-e> :NERDTreeToggle<CR>

" formatoptions
set formatoptions-=o " Don't open comment on o or O

" foldmethod
set foldmethod=syntax

" Colorscheme
colorscheme molokai

" Listchars, to show nbsp
set listchars=nbsp:¬,eol:¶,tab:>-,extends:»,precedes:«,trail:•

"VAM
" put this line first in ~/.vimrc
set nocompatible | filetype indent plugin on | syn on

fun! EnsureVamIsOnDisk(plugin_root_dir)
  " windows users may want to use http://mawercer.de/~marc/vam/index.php
  " to fetch VAM, VAM-known-repositories and the listed plugins
  " without having to install curl, 7-zip and git tools first
  " -> BUG [4] (git-less installation)
  let vam_autoload_dir = a:plugin_root_dir.'/vim-addon-manager/autoload'
  if isdirectory(vam_autoload_dir)
    return 1
  else
    if 1 == confirm("Clone VAM into ".a:plugin_root_dir."?","&Y\n&N")
      " I'm sorry having to add this reminder. Eventually it'll pay off.
      call confirm("Remind yourself that most plugins ship with ".
                  \"documentation (README*, doc/*.txt). It is your ".
                  \"first source of knowledge. If you can't find ".
                  \"the info you're looking for in reasonable ".
                  \"time ask maintainers to improve documentation")
      call mkdir(a:plugin_root_dir, 'p')
      execute '!git clone --depth=1 git://github.com/MarcWeber/vim-addon-manager '.
                  \       shellescape(a:plugin_root_dir, 1).'/vim-addon-manager'
      " VAM runs helptags automatically when you install or update 
      " plugins
      exec 'helptags '.fnameescape(a:plugin_root_dir.'/vim-addon-manager/doc')
    endif
    return isdirectory(vam_autoload_dir)
  endif
endfun

fun! SetupVAM()
  " Set advanced options like this:
  " let g:vim_addon_manager = {}
  " let g:vim_addon_manager.key = value
  "     Pipe all output into a buffer which gets written to disk
  " let g:vim_addon_manager.log_to_buf =1

  " Example: drop git sources unless git is in PATH. Same plugins can
  " be installed from www.vim.org. Lookup MergeSources to get more control
  " let g:vim_addon_manager.drop_git_sources = !executable('git')
  " let g:vim_addon_manager.debug_activation = 1

  " VAM install location:
  let c = get(g:, 'vim_addon_manager', {})
  let g:vim_addon_manager = c
  let c.plugin_root_dir = expand('$HOME/.vim/vim-addons')
  if !EnsureVamIsOnDisk(c.plugin_root_dir)
    echohl ErrorMsg | echomsg "No VAM found!" | echohl NONE
    return
  endif
  let &rtp.=(empty(&rtp)?'':',').c.plugin_root_dir.'/vim-addon-manager'

  " Tell VAM which plugins to fetch & load:
  call vam#ActivateAddons(['github:honza/vim-snippets','github:garbas/vim-snipmate','surround','fugitive','minibufexplorer','github:valloric/youcompleteme','afterimage','github:scrooloose/nerdtree','github:kien/ctrlp.vim','ag','github:mattn/gist-vim','github:mattn/webapi-vim','github:godlygeek/tabular'], {'auto_install' : 0})
  " sample: call vam#ActivateAddons(['pluginA','pluginB', ...], {'auto_install' : 0})

  " Addons are put into plugin_root_dir/plugin-name directory
  " unless those directories exist. Then they are activated.
  " Activating means adding addon dirs to rtp and do some additional
  " magic

  " How to find addon names?
  " - look up source from pool
  " - (<c-x><c-p> complete plugin names):
  " You can use name rewritings to point to sources:
  "    ..ActivateAddons(["github:foo", .. => github://foo/vim-addon-foo
  "    ..ActivateAddons(["github:user/repo", .. => github://user/repo
  " Also see section "2.2. names of addons and addon sources" in VAM's documentation
endfun
call SetupVAM()
" experimental [E1]: load plugins lazily depending on filetype, See
" NOTES
" experimental [E2]: run after gui has been started (gvim) [3]
" option1:  au VimEnter * call SetupVAM()
" option2:  au GUIEnter * call SetupVAM()
" See BUGS sections below [*]
" Vim 7.0 users see BUGS section [3]

" MiniBufExplorer Options
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapCTabSwitchBufs = 1
" Buffer List
" map <F4> :ls<CR>:buffer<space>
" Enable mouse
set mouse=a

" Ag Options
let g:aghighlight=1

" ctrlp config
let g:ctrlp_map = '<leader>f'
let g:ctrlp_max_height = 30
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window_reversed = 0
