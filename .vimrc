set encoding=utf-8
set fencs=utf-8

set nu
set autoindent
set ts=2
set shiftwidth=2
au BufReadPost *
            \ if line ("'\"") > 0 && line ("'\"") <= line ("$") | 
            \ exe "norm g'\"" |
            \ endif

set showmatch
set wmnu
set title
set incsearch
syntax on
filetype indent on
set showcmd
set cursorline
"set cursorcolumn

set virtualedit=all
