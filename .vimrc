set nu
"set relativenumber
set autoindent
set ts=4
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab
au BufReadPost *
            \ if line ("'\"") > 0 && line ("'\"") <= line ("$") | 
            \ exe "norm g'\"" |
            \ endif
set laststatus=2
set showmatch
set wmnu
set title
set incsearch

set list listchars=tab:>-,trail:·

syntax on
filetype on
filetype indent on
set encoding=utf-8
set fencs=utf-8
set showcmd
set cursorline
"set cursorcolumn

set virtualedit=all
